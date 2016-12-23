module Main where

import Perft
import SpeedTest
import Analyze
import HashKeyTest
import Engine
import Control.Applicative
import Options
import Data.Maybe
import Data.Either
import Control.Monad
import System.IO

data Mode   = Perft
            | SpeedTest
            | Analyze
            | HashTest
            | Engine
    deriving (Bounded,Enum,Show)

data MainOptions = MainOptions {
        mode    :: Mode
    ,   depth   :: Int
    ,   fen     :: String
    ,   timeout :: Int
}

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> defineOption (optionType_enum "mode") (\o -> o
                {   optionShortFlags    = ['m']
                ,   optionLongFlags     = ["mode"]
                ,   optionDefault       = Engine
                })
        <*> defineOption optionType_int (\o -> o
                {   optionShortFlags    = ['d']
                ,   optionLongFlags     = ["depth"]
                ,   optionDefault       = 10
                ,   optionDescription   = "The depth to which search or analysis goes on."
                })
        <*> defineOption optionType_string (\o -> o
                {   optionShortFlags    = ['f']
                ,   optionLongFlags     = ["fen"]
                ,   optionDefault       = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
                ,   optionDescription   = "The position from which the search or analysis starts."
                })
        <*> defineOption optionType_int (\o -> o
                {   optionShortFlags    = ['t']
                ,   optionLongFlags     = ["timeout"]
                ,   optionDefault       = 1000
                ,   optionDescription   = "The time to wait for the end of the computation(milliseconds)."
                })

main :: IO ()
main = runCommand $ \opts args -> case mode opts of
    Perft       -> perft (depth opts)
    SpeedTest   -> speedtest (depth opts) (fen opts)
    Analyze     -> analyze (depth opts) (fen opts) (timeout opts)
    HashTest    -> hashKeyTest (depth opts) 100000
    Engine      -> do
        hSetBuffering stdout NoBuffering
        launchEngine
