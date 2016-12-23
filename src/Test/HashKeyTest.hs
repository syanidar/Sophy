module HashKeyTest where

import Alias
import BasicData
import Position
import Move
import Constructor
import System.Random
import Control.Monad
import Data.Maybe

nextRandomPos :: Position -> IO Position
nextRandomPos x = do
    let moves       = legalMoves x
        numMoves    = length moves
    if numMoves == 0
    then return x
    else flip makeMove x . (moves !!) . (`mod` numMoves) <$> randomIO

randomPosAt :: Depth -> Position -> IO Position
randomPosAt n = randomPosAt'
    where   randomPosAt' = foldl1 (>=>) $ replicate n nextRandomPos

hashMatch :: Position -> Bool
hashMatch x = _zobristKey x == calcHashKey x

hashKeyTest :: Depth -> Int -> IO ()
hashKeyTest x y = do
    let genRandomPos =  replicateM y $ randomPosAt x
    err <- listToMaybe . filter (not . hashMatch) <$> sequence (genRandomPos startPos)
    when (isJust err) $ do
        putStrLn "Hash didn't match in the following position"
        let pos = fromMaybe (error "err can't be Nothing!!") err
        putStrLn $ showPos pos
