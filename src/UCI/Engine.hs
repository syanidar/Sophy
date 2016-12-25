{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
module Engine where

import Alias
import BasicData
import Position
import Move
import Interpreter
import UCIData
import Search
import Data.Maybe
import Control.Monad.State.Strict
import Control.Monad
import Control.Lens
import Control.Exception hiding(handle)
import Control.Concurrent
import Control.Concurrent.STM
import System.IO

data EngineState = EngineState {    _currentPosition    :: Maybe Position
                                ,   _currentResult      :: TVar (Evaluation,SearchState)
                                ,   _currentSearch      :: Maybe ThreadId}
makeLenses '' EngineState

type Engine a = StateT EngineState IO a

launchEngine :: IO ()
launchEngine = do
    result <- atomically $ newTVar (error "No position has been evaluated.")
    evalStateT waitInput EngineState{   _currentPosition    = Nothing
                                    ,   _currentResult      = result
                                    ,   _currentSearch      = Nothing}

waitInput :: Engine ()
waitInput = do
    command <- uciCommandFrom <$> liftIO getLine
    handle command

handle :: UCICommand -> Engine ()
handle UCI = do
    liftIO . putStrLn $ "id name Sophy"
    liftIO . putStrLn $ "id author Teguramori Ryo"
    -- options here
    liftIO . putStrLn $ "uciok"
    liftIO . hFlush $ stdout
    waitInput
handle IsReady = do
    liftIO . putStrLn $ "readyok"
    liftIO . hFlush $ stdout
    waitInput
handle (InPosition pos) = do
    modify' (currentPosition .~ Just pos)
    waitInput
handle (Go tokens) = do
    pos         <- fromMaybe (error "Position isn't loaded yet.") <$> gets _currentPosition
    env         <- liftIO $ environment 1000000
    result      <- liftIO . atomically $ newTVar (error "Timeout without evaluating anything.")
    modify' (currentResult .~ result)
    terminated  <- liftIO $ registerDelay (1000 * timeoutFrom pos tokens)

    search <- liftIO . forkIO $ do
        let analyze             = search (iddfs (depthFrom tokens) (Just result) pos) env
            terminate (Left x)  = flip throwTo x =<< myThreadId
            terminate (Right _) = atomically $ writeTVar terminated True
        analyzer <- analyze `forkFinally` terminate
        atomically $ readTVar terminated >>= check
        killThread analyzer
        showResult =<< readTVarIO result
    modify' (currentSearch .~ Just search)
    waitInput
handle Stop = do
    thread  <- gets _currentSearch
    liftIO $ maybe (return ()) killThread thread
    tvar    <- gets _currentResult
    result  <- liftIO $ readTVarIO tvar
    liftIO $ showResult result
    waitInput
handle Quit = return ()
handle _ = waitInput

showResult :: (Evaluation,SearchState) -> IO ()
showResult (evaluation,SearchState{_variation = (best:ponder:_)}) = do
    putStrLn $ "bestmove " ++ toLAN best ++ " ponder " ++ toLAN ponder
    hFlush stdout
showResult (evaluation,SearchState{_variation = (best:_)}) = do
    putStrLn $ "bestmove " ++ toLAN best
    hFlush stdout

timeoutFrom :: Position -> [GoToken] -> Int
timeoutFrom _ []                = error "No information about the remaining time."
timeoutFrom _ (MoveTime x:_)    = x
timeoutFrom _ (Infinite:_)      = 1000000000
timeoutFrom pos (WTime x:xs)    = if isWhite pos
                                    then x `div` max 25 (40 - (pos ^. plyCount `div` 2))
                                    else timeoutFrom pos xs
timeoutFrom pos (BTime x:xs)    = if isBlack pos
                                    then x `div` max 25 (40 - (pos ^. plyCount `div` 2))
                                    else timeoutFrom pos xs
timeoutFrom pos (_:xs)          = timeoutFrom pos xs

depthFrom :: [GoToken] -> Int
depthFrom [] = 100
depthFrom (DepthLimit x:_) = x
depthFrom (_:xs) = depthFrom xs
