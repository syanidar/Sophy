module Perft where

import Prelude hiding((!!),foldl1)
import Move
import Position
import BasicData
import Constructor
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.List.Safe
import Data.Maybe

perft :: Int -> IO ()
perft x = do
    perftEntries <- either (error.show) id <$> parseFromFile perftParser "perftsuite.epd"
    mapM_ perft' perftEntries
    putStrLn "Perft suite completed."
    where
        perft' (position,answers) = do
            let result  = length . extract $ position
                answer  = answers !! (x - 1)
            when (maybe False (/= result) answer) $
                putStrLn    $   "Perft failed in position \n"
                            ++  showPos position ++ "\n"
                            ++  "Expected:      " ++ show answer ++ "\n"
                            ++  "Calculated:    " ++ show result ++ "\n"
        extract             = fromMaybe (error "Depth must be greater than 0.") . foldl1 (>=>) . replicate x $ legalPositions

perftParser :: Parser [(Position,[Int])]
perftParser = many1 onePosition
    where
        onePosition = do
            fen     <- anyChar `manyTill` char ';'
            count   <- nodeCount `sepBy1` char ';'
            return (fromFEN fen,count)
        nodeCount = do
            char 'D'
            digit
            space
            read <$> manyTill digit (void space <|> void newline <|> eof)
