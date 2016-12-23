module Analyze where

import Alias
import BasicData
import Position
import Move
import Constructor
import Search
import Control.Monad.State
import Control.Lens
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM

analyze :: Depth -> FEN -> Timeout -> IO ()
analyze x y z = do
    let pos = fromFEN y
    putStrLn    $   "Analyzing in position,\n"
                ++  showPos pos                 ++ "\n"
    env        <- environment 1000000
    result      <- atomically $ newTVar (error "Timeout without evaluating anything.")
    terminated  <- registerDelay (z * 1000)

    let analyze             = search (iddfs x (Just result) pos) env
        terminate (Left x)  = flip throwTo x =<< myThreadId
        terminate (Right _) = atomically $ writeTVar terminated True

    analyzer <- analyze `forkFinally` terminate

    atomically $ readTVar terminated >>= check

    (evaluation,state)  <- readTVarIO result
    killThread analyzer
    putStrLn    $   "NODE:          " ++ show (state ^. counter)                ++ "\n"
                ++  "VARIATION:     " ++ show (map toLAN (state ^. variation))  ++ "\n"
                ++  "EVALUATION:    " ++ show evaluation
