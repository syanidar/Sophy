module SpeedTest where

import Prelude hiding(foldl1)
import Position
import Constructor
import Move
import Data.Time
import Control.Monad
import Data.Maybe
import Data.List.Safe

speedtest :: Int -> String -> IO ()
speedtest x y = do
    let extract     = fromMaybe (error "Depth must be greater than 0.") . foldl1 (>=>) . replicate x $ legalPositions
        nodeCount   = length . extract $ fromFEN y
    start       <- getCurrentTime
    putStrLn    $   "NODE:              " ++ show nodeCount
    end         <- getCurrentTime
    let elapse = fromEnum (diffUTCTime end start) `div` 1000000000
    putStrLn    $   "TIME(MILLISECOND): " ++ show elapse
    putStrLn    $   "NPS:               " ++ show (nodeCount * 1000 `div` elapse)
