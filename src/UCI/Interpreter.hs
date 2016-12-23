module Interpreter where


import Move
import Constructor
import UCIData
import System.IO
import Control.Monad
import Data.List
import Data.Composition
import Data.Maybe

uciCommandFrom :: String -> UCICommand
uciCommandFrom = genCommand.words
    where
        genCommand ("uci":_)                = UCI
        genCommand ("debug":"on":_)         = Debug True
        genCommand ("debug":"off":_)        = Debug False
        genCommand ("isready":_)            = IsReady
        genCommand ("setoption":"name":xs)  = SetOption name value
            where
                (names,values) = break (== "value") xs
                name    = unwords names
                value   = stripPrefix "value " (unwords values)
        genCommand ("register":"later":_)   = RegisterLater
        genCommand ("register":"name":xs)   = Register (unwords names) (read code)
            where
                (names,["code",code]) = break (== "code") xs
        genCommand ("ucinewgame":_)         = UCINewGame
        genCommand ("position":xs)          = InPosition (playMoves position)
            where
                (fen,moves) = break (== "moves") xs
                position = maybe startPos fromFEN $ stripPrefix "fen " (unwords fen)
                playMoves = fromMaybe (error "Failed to parse moves") .: foldl (>=>) return  . map play $ filter (/= "moves") moves
        genCommand ("go":xs)                = Go (genTokens [] xs)
            where
                genTokens tokens []                  = tokens
                genTokens tokens ("searchmoves":ys)  = SearchMoves ys:tokens
                genTokens tokens ("ponder":ys)       = genTokens (Ponder:tokens) ys
                genTokens tokens ("infinite":ys)     = genTokens (Infinite:tokens) ys
                genTokens tokens (token:n:ys)   = let number = read n in case token of
                    "wtime"     -> genTokens (WTime number:tokens) ys
                    "btime"     -> genTokens (BTime number:tokens) ys
                    "winc"      -> genTokens (WInc number:tokens) ys
                    "binc"      -> genTokens (BInc number:tokens) ys
                    "movestogo" -> genTokens (MovesToGO number:tokens) ys
                    "depth"     -> genTokens (DepthLimit number:tokens) ys
                    "nodes"     -> genTokens (NodeLimit number:tokens) ys
                    "mate"      -> genTokens (Mate number:tokens) ys
                    "movetime"  -> genTokens (MoveTime number:tokens) ys
        genCommand ("stop":_)               = Stop
        genCommand ("ponderhit":_)          = PonderHit
        genCommand ("quit":_)               = Quit
