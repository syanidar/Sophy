{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}
module Search where

import Alias
import BasicData
import Position
import Move
import Evaluation
import Data.List
import Data.Function
import Data.Maybe
import Data.Bits
import qualified Data.Vector.Mutable as TT
import qualified Data.IntMap.Strict as Map
import Data.Time
import Data.Composition
import Data.IORef
import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Arrow hiding(left)
import Control.Lens
import qualified Control.Exception as E
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans.Either
import System.IO

data HashEntry = HashEntry{     posId       :: ZobristKey
                            ,   priority    :: Depth
                            ,   score       :: Evaluation
                            ,   scoreType   :: ScoreType
                            ,   refutation  :: Maybe Move}

type HashTable          = TT.IOVector HashEntry

data SearchState        = SearchState{
        _counter        :: Counter
    ,   _variation      :: Variation
    ,   _killers        :: Map.IntMap Killers}
makeLenses '' SearchState

data SearchEnvironment  = SearchEnvironment{
        _table          :: HashTable}
makeLenses '' SearchEnvironment

data NodeType = PV | Cut | All

type Search = EitherT Evaluation (StateT SearchState (ReaderT SearchEnvironment IO))

search :: Search Evaluation -> SearchEnvironment -> IO (Evaluation,SearchState)
search x = runReaderT (runStateT (either id id <$> runEitherT x) initSearchState)

say :: String -> Search ()
say = liftIO . putStrLn

minEval,maxEval,defeat:: Evaluation
minEval         = -1000000
maxEval         = 1000000
defeat          = -(material King)

initSearchState :: SearchState
initSearchState = SearchState{  _counter        = 0
                            ,   _variation      = []
                            ,   _killers        = Map.empty}

declare :: a -> Search (IORef a)
declare = liftIO . newIORef

assign :: IORef a -> a -> Search ()
assign = liftIO .: writeIORef

apply :: (a -> a) -> IORef a -> Search ()
apply = liftIO .: flip modifyIORef'

refer :: IORef a -> Search a
refer = liftIO . readIORef

environment :: TableSize -> IO SearchEnvironment
environment x = do
    tt  <- TT.new x
    return SearchEnvironment{   _table = tt}

tickCounter :: Search ()
tickCounter = modify' (counter %~ (+ 1))

getVariation :: Search Variation
getVariation = gets _variation

clearVariation :: Search Variation
clearVariation = do
    prevVal <-  getVariation
    modify' (variation .~ [])
    return prevVal

setVariation :: Variation -> Search ()
setVariation xs = modify' (variation .~ xs)

putMove :: Move -> Search ()
putMove x = modify' (variation %~ (x:))

putKiller :: Depth -> Move -> Position -> Search ()
putKiller n x y = when (isQuiet y x) $ modify' (killers %~ updateKillersAtTheDepth)
    where
        updateKillersAtTheDepth     = Map.alter updateKillers n
        updateKillers Nothing       = Just [x]
        updateKillers (Just [y])    = Just [x,y]
        updateKillers (Just ys@[better,worse]) = if x /= better
            then Just [x,better]
            else Just ys

getSelectedMove :: Search (Maybe Move)
getSelectedMove = listToMaybe <$> getVariation

getKillers :: Depth -> Search [Move]
getKillers n = fromMaybe [] . Map.lookup n <$> gets _killers

indexPos :: Position -> Search Int
indexPos x = do
    tableSize <- TT.length <$> asks _table
    return $ fromIntegral (_zobristKey x) `mod` tableSize

lookupTT :: HashTable -> Int -> Search (Maybe HashEntry)
lookupTT x y = liftIO $ E.catch action handler
    where
        action                  = fmap Just $ E.evaluate =<< TT.unsafeRead x y
        handler (E.ErrorCall _) = return Nothing

samePos :: Position -> HashEntry -> Bool
samePos x y = _zobristKey x == posId y

getHash :: Position -> Search (Maybe HashEntry)
getHash x = do
    ix      <- indexPos x
    entry   <- (`lookupTT` ix) =<< asks _table
    return  $  mfilter (samePos x) entry

putHash :: Position -> Maybe HashEntry -> HashEntry -> Search ()
putHash x y z = do
    let putEntry e = do
            ix <- indexPos x
            tt <- asks _table
            liftIO $ TT.unsafeWrite tt ix e
        select new old = if ((>=) `on` priority) new old then new else old

    putEntry $ maybe z (select z) y

updateBounds :: Depth -> Alpha -> Beta -> Maybe HashEntry -> (Alpha,Beta)
updateBounds n alpha beta entry = case mfilter ((>= n) . priority) entry of
    Nothing                                             -> (alpha,beta)
    Just HashEntry{scoreType = Exact,score = x}         -> (x,x)
    Just HashEntry{scoreType = LowerBound,score = x}    -> (max alpha x,beta)
    Just HashEntry{scoreType = UpperBound,score = x}    -> (alpha,min beta x)

verifyCutoff::  Monad m => EitherT a m a -> EitherT a m a
verifyCutoff = lift . fmap (either id id) . runEitherT

cutoff :: Monad m => e -> EitherT e m a
cutoff = left

negaScout :: Depth -> Alpha -> Beta -> Bool -> Bool -> Position -> Search Evaluation
negaScout n alpha beta pvNode preventNullMove position = verifyCutoff $ do
    let moves = legalMoves position

    --  One has to return either the mate score or the draw score(== 0) if there is no legal moves.
    when (null moves) $ do
        tickCounter
        if isInCheck position
        then cutoff (defeat + position ^. plyCount)
        else cutoff 0

    --  The game will be drawn if there isn't sufficient material to deliver mate.
    when (insufficientMaterial  position) $ do
        tickCounter
        cutoff 0

    --  If the side to play has the right to draw, the engine always return the draw score.
    --  Note that much remains to be improved here, because you would not accept a draw if you have a supperior position.
    --  I'm going to reflect this in the future, but not now.
    when (canDraw position) $ do
        tickCounter
        cutoff 0

    --  When depth <= 0, the search gets into the quiescence search, unless the side to move is not in check.
    --  The reason why we extend such positions is that being in check means we are in a forcing sequence.
    --  Thus we want to know the outcome after the sequence rathar than the evaluation for the current situation.
    when (n <= 0) $ if isInCheck position
        then cutoff =<< negaScout 1 alpha beta pvNode preventNullMove position
        else cutoff =<< quiescence alpha beta position

    --  Here is a big saving comes in. Null Move Pruning!!
    --  We don't try to null move if the evaluation of the current position is lower than beta, because it's unlikely to fail high in that case.
    unless (preventNullMove || pvNode || isInCheck position || endGame position) $ do
        let standPad = evaluate position
        when (standPad >= beta) $ do
            score <- negate <$> negaScout (n - 3) (-beta) (-beta + 1) False True (pass position)
            when (score >= beta) $ cutoff score

    --  We now refer to the previous evaluation of the same position stored in the hash table.
    --  Not only could we return early if the value in the table matchs certain criteria,
    --  we could also narrow the window if the depth in which the position was stored in the table is deeper than or equal to that of the current position.
    entry   <- getHash position
    let hashMove        = intersect moves . maybeToList $ refutation =<< entry
        (alpha',beta')  = if pvNode then (alpha, beta) else updateBounds n alpha beta entry

    when (alpha' >= beta') $ do
        setVariation hashMove
        cutoff $ maybe (error "No hash entry.") score entry

    --  We could still get a good move ordering by getting the best move at a shallower depth even if we couldn't have obtained the hash move.
    --  This is what's called internal iterative deepning.
    iidMove <- if null hashMove && n > 4 && pvNode
        then do
            negaScout (n - 4) alpha' beta' True True position
            result <- getSelectedMove
            clearVariation
            return $ maybeToList result
        else return hashMove

    --  Fetching killer moves that are legal in this position.
    killers <- (\\ hashMove) . intersect moves <$> getKillers n

    let (tactical,quiet)                = partition (isTactical position) $ moves \\ (hashMove ++ killers)
        (winningTactics,losingCaptures) = partition (`elem` noisyMoves position) tactical
        ordered                         = iidMove ++ winningTactics ++ killers ++ losingCaptures ++ quiet
    --  The leftmost nodes should always be searched with full window.
    pvScore         <-  do
        let pvMove = head ordered
        score <- negate <$> negaScout (n - 1) (-beta') (-alpha') pvNode False (makeMove pvMove position)
        putMove pvMove
        putKiller n pvMove position
        when (score >= beta') $ cutoff score
        return score

    --  Other nodes are first analyzed by so called scout search in which the null window is used in order to prove that the node fails low.
    --  However if the scout search fails high, we have to do a research with full window.
    let
        negaScout' localAlpha _ []              = return localAlpha
        negaScout' localAlpha moveCount (x:xs)  = do
            variation   <- clearVariation

            score <- verifyCutoff $ do
                --  If we are not in the pv node i.e. we are not interested in the minimax value of the position, we could reduce the search depth of later moves without significant loss of accuracy.
                --  This is how late move reduction(LMR) works.
                unless (isTactical position x || isCheck position x || isInCheck position || pvNode) $ do
                    let reduction   = truncate $ sqrt (fromIntegral $ n - 1) + sqrt (fromIntegral $ moveCount - 1)
                    lmrScore <- negate <$> negaScout (n - 1 - reduction) (-localAlpha - 1) (-localAlpha) False False (makeMove x position)
                    when (lmrScore <= localAlpha) $ cutoff lmrScore

                nwsScore <- negate <$> negaScout (n - 1) (-localAlpha - 1) (-localAlpha) False False (makeMove x position)
                if localAlpha < nwsScore && nwsScore < beta'
                then negate <$> negaScout (n - 1) (-beta') (-localAlpha) True False (makeMove x position)
                else return nwsScore

            if score >= beta'  then do
                putMove x
                putKiller n x position
                return score
            else if score > localAlpha then do
                putMove x
                negaScout' score (moveCount + 1) xs
            else do
                setVariation variation
                negaScout' localAlpha (moveCount + 1) xs

    score           <-  negaScout' (max pvScore alpha') 1 (tail ordered)
    moveSelected    <-  getSelectedMove

    --  We store the information obtained while analyzing this position into the table so that we could use it if we hit the same position in the future search.
    let sType   | score <= alpha'   = UpperBound
                | score >= beta'    = LowerBound
                | otherwise         = Exact

    putHash position entry HashEntry{   posId       = _zobristKey position
                                    ,   priority    = max 0 n
                                    ,   score       = score
                                    ,   scoreType   = sType
                                    ,   refutation  = moveSelected}
    return score

quiescence :: Alpha -> Beta -> Position -> Search Evaluation
quiescence alpha beta position =  verifyCutoff $ do
    when (null (legalMoves position)) $ do
        tickCounter
        if isInCheck position
        then cutoff (defeat + position ^. plyCount)
        else cutoff 0

    entry <- getHash position

    let
        (alpha',beta') = updateBounds 0 alpha beta entry
        standPad = evaluate position

    tickCounter
    when (standPad >= beta') $ cutoff standPad


    let moves       = noisyMoves position
        hashMove    = filter (`elem` moves) . maybeToList $ refutation =<< entry

    when (alpha' >= beta') $ do
        setVariation hashMove
        cutoff $ maybe (error "No hash entry.") score entry


    let quiescence' localAlpha []      = return localAlpha
        quiescence' localAlpha (x:xs)  = do
            variation   <- clearVariation
            score       <- negate <$> quiescence (-beta') (-(max localAlpha alpha')) (makeMove x position)
            if score >= beta' then do
                putMove x
                return score
            else if score > localAlpha then do
                putMove x
                quiescence' score xs
            else do
                setVariation variation
                quiescence' localAlpha xs

    score           <-  quiescence' standPad (hashMove ++ (moves \\ hashMove))
    moveSelected    <-  getSelectedMove

    let sType   | score <= alpha'   = UpperBound
                | score >= beta'    = LowerBound
                | otherwise         = Exact

    putHash position entry HashEntry{   posId       = _zobristKey position
                                    ,   priority    = 0
                                    ,   score       = score
                                    ,   scoreType   = sType
                                    ,   refutation  = moveSelected}
    return score

mtdf :: Depth -> Evaluation -> Position -> Search Evaluation
mtdf n firstGuess position = mtdf' firstGuess minEval maxEval
    where
        mtdf' guess lower upper
            | lower < upper = do
                let beta = max (lower + 1) guess
                eval <- negaScout n (beta - 1) beta True False position
                if eval < beta
                then mtdf' eval lower eval
                else mtdf' eval eval upper
            | otherwise     = return guess

aspirationSearch :: Depth -> Evaluation -> Position -> Search Evaluation
aspirationSearch n guess position = aspire (guess - 25) (guess + 25)
    where
        aspire lower upper = do
            eval <- negaScout n lower upper True False position
            let size = upper - lower
            if eval >= upper         then   aspire lower (upper + size)
            else if eval <= lower    then   aspire (lower - size) upper
            else                            return eval

iddfs :: Depth -> Maybe (TVar (Evaluation,SearchState)) -> Position -> Search Evaluation
iddfs n result position = iddfs' 1 0 0
    where
        update :: Evaluation -> Search ()
        update !x = do
            !state <- get
            liftIO . atomically $ maybe (return ()) (`writeTVar` (x,state)) result
        iddfs' m prevEval guess = do
            score <- aspirationSearch m guess position
            uciInfo m score position
            update score
            iddfs' (m + 1) score prevEval

uciInfo :: Depth -> Evaluation -> Position -> Search ()
uciInfo n score position = do
    SearchState{_counter = nodes,_variation = pv} <- get
    let mate        = abs score > abs defeat `div` 2
        scoreString = if mate
        then "mate "    ++ show (signum score * (matePly `div` 2 + 1))
        else "cp "      ++ show score
        matePly  = abs defeat - abs score - position ^. plyCount
    say $ "info depth " ++ show n ++ " nodes " ++ show nodes ++ " score " ++ scoreString ++ " pv " ++ unwords (map toLAN pv)
    liftIO $ hFlush stdout
