{-# LANGUAGE Strict #-}
module Evaluation where

import Alias
import Bitwise
import BasicData
import Move
import Position
import BitBoard
import qualified AttackTable as T
import Data.Bits
import Control.Lens hiding(without)

evaluate,isolatedPawns,space,openFiles,openADiagonals,openDDiagonals,outpost,kingDefenders,tropism :: Position -> Evaluation
evaluate pos
    =   (pos^.materialBalance)
    +   (if isWhite pos then positionEval else -positionEval)
    +   space pos
    -- +   mobility pos
    +   tropism pos
    -- +   openFiles pos
    -- +   openADiagonals pos
    -- +   openDDiagonals pos
    +   (if endGame pos then 0 else kingDefenders pos)
    -- +   outpost pos
    -- -   isolatedPawns pos
    where
        (wp,wn,wb,wr,wq,wk,bp,bn,bb,br,bq,bk) = if isWhite pos
        then (pos<-!->Pawn,pos<-!->Knight,pos<-!->Bishop,pos<-!->Rook,pos<-!->Queen,pos<-!->King,pos<~!~>Pawn,pos<~!~>Knight,pos<~!~>Bishop,pos<~!~>Rook,pos<~!~>Queen,pos<~!~>King)
        else (pos<~!~>Pawn,pos<~!~>Knight,pos<~!~>Bishop,pos<~!~>Rook,pos<~!~>Queen,pos<~!~>King,pos<-!->Pawn,pos<-!->Knight,pos<-!->Bishop,pos<-!->Rook,pos<-!->Queen,pos<-!->King)
        positionEval    =   pawnPosition wp bp
                        +   knightPosition wn bn
                        +   bishopPosition wb bb
                        +   rookPosition wr br
                        +   queenPosition wq bq
                        +   (if endGame pos then kingEPosition wk bk else kingPosition wk bk)
mobility pos = length (legalMoves pos) - length (legalMoves (pass pos))
isolatedPawns pos = (popCount isolatedAttacker - popCount isolatedDefender) * 25
    where
        isolatedAttacker = p `without` fillForward (pawnAttack color p)
        isolatedDefender = p' `without` fillBackward (pawnAttack (opposite color) p')
        fillForward     = if color == White then fill North else fill South
        fillBackward    = if color == White then fill South else fill North
        p  = pos<-!->Pawn
        p' = pos<~!~>Pawn
        color = pos^.activeColor
space pos = popCount (attackMap pos .&. dTerritory)  - popCount (_defendMap pos .&. aTerritory)
    where
        attackMap pos@Position{_attackers = a,_bishops = b,_rooks = r,_queens = q}
            =   pawnAttack (pos^.activeColor) (pos<-!->Pawn)
            .|. knightAttack (pos<-!->Knight)
            .|. bishopAttack (occupancy pos) ((b.|.q).&.a)
            .|. rookAttack (occupancy pos) ((r.|.q).&.a)
            .|. kingAttack (pos<-!->King)
        (aTerritory,dTerritory) = if isWhite pos then (0x00000000ffffffff,0xffffffff00000000) else (0xffffffff00000000,0x00000000ffffffff)
openFiles pos = (popCount (openFiles `without` openFiles') - popCount (openFiles' `without` openFiles)) `div` 8 * 20
    where
        openFiles     = fillVertically (pos<-!->Pawn)
        openFiles'    = fillVertically (pos<~!~>Pawn)
openADiagonals pos = (popCount (openDiagonals `without` openDiagonals') - popCount (openDiagonals' `without` openDiagonals)) `div` 8 * 10
    where
        p   = pos<-!->Pawn
        p'  = pos<~!~>Pawn
        openDiagonals     = fill NorthEast p .|. fill SouthWest p
        openDiagonals'    = fill NorthEast p' .|. fill SouthWest p'
openDDiagonals pos = (popCount (openDiagonals `without` openDiagonals') - popCount (openDiagonals' `without` openDiagonals)) `div` 8 * 10
    where
        p   = pos<-!->Pawn
        p'  = pos<~!~>Pawn
        openDiagonals     = fill NorthWest p .|. fill SouthEast p
        openDiagonals'    = fill NorthWest p' .|. fill SouthEast p'

outpost pos = (popCount goodKnights - popCount goodKnights') * 10
    where
        goodKnights     = n .&. pawnAttack color (pos<-!->Pawn) .&. halfOpenFiles
        goodKnights'    = n' .&. pawnAttack (opposite color) (pos<~!~>Pawn) .&. halfOpenFiles'
        halfOpenFiles   = fillVertically p' `without` fillVertically p
        halfOpenFiles'  = fillVertically p `without` fillVertically p'
        p               = pos<-!->Pawn
        p'              = pos<~!~>Pawn
        n               = pos<-!->Knight
        n'              = pos<~!~>Knight
        (aTerritory,dTerritory) = if isWhite pos then (0x00000000ffffffff,0xffffffff00000000) else (0xffffffff00000000,0x00000000ffffffff)
        color = pos^.activeColor
kingDefenders pos =  (defendedPawnAttackers - defendedPawnDefenders) * 25
    where
        defendedPawnAttackers
            =   popCount
            .   pawnAttack (pos^.activeColor)
            $   T.kingAttack (countTrailingZeros $ pos<-!->King) .&. (pos<-!->Pawn)
        defendedPawnDefenders
            =   popCount
            .   pawnAttack (pos^.activeColor&opposite)
            $   T.kingAttack (countTrailingZeros $ pos<~!~>King) .&. (pos<~!~>Pawn)
tropism pos = (attackerTropism - defenderTropism) `div` 50
    where
        kSquare     = countTrailingZeros $ pos<-!->King
        kFile       = kSquare `mod` 8
        kRank       = kSquare `div` 8
        kSquare'    = countTrailingZeros $ pos<-!->King
        kFile'      = kSquare' `mod` 8
        kRank'      = kSquare' `div` 8
        attackerTropism = sum $ do
            piece       <- [Pawn .. Queen]
            location    <- collapse $ pos<-!->piece
            let square      = countTrailingZeros location
                file        = square `mod` 8
                rank        = square `div` 8
                distance    = abs (kFile' - file) ^ 2 + abs (kRank' - rank) ^ 2
            return $ material piece `div` distance
        defenderTropism = sum $ do
            piece       <- [Pawn .. Queen]
            location    <- collapse $ pos<~!~>piece
            let square      = countTrailingZeros location
                file        = square `mod` 8
                rank        = square `div` 8
                distance    = abs (kFile - file) ^ 2 + abs (kRank - rank) ^ 2
            return $ material piece `div` distance
pawnPosition,knightPosition,bishopPosition,rookPosition,queenPosition,kingPosition :: BitBoard -> BitBoard -> Int
pawnPosition whites blacks  =   popCount (whites .&. pawnBonus50) * 50
                            +   popCount (whites .&. pawnBonus30) * 30
                            +   popCount (whites .&. pawnBonus25 ) * 25
                            +   popCount (whites .&. pawnBonus20 .|. blacks .&. invert pawnPenalty20) * 20
                            +   popCount (whites .&. pawnBonus10 .|. blacks .&. invert pawnPenalty10) * 10
                            +   popCount (whites .&. pawnBonus5 .|. blacks .&. invert pawnPenalty5) * 5
                            -   popCount (whites .&. pawnPenalty5 .|. blacks .&. invert pawnBonus5) * 5
                            -   popCount (whites .&. pawnPenalty10 .|. blacks .&. invert pawnBonus10) * 10
                            -   popCount (whites .&. pawnPenalty20 .|. blacks .&. invert pawnBonus20) * 20
                            -   popCount (blacks .&. invert pawnBonus25) * 25
                            -   popCount (blacks .&. invert pawnBonus30) * 30
                            -   popCount (blacks .&. invert pawnBonus50) * 50
knightPosition whites blacks    =   popCount (blacks .&. invert knightPenalty50) * 50
                                +   popCount (blacks .&. invert knightPenalty40) * 40
                                +   popCount (blacks .&. invert knightPenalty30) * 30
                                +   popCount (whites .&. knightBonus20 .|. blacks .&. invert knightPenalty20) * 20
                                +   popCount (whites .&. knightBonus15) * 15
                                +   popCount (whites .&. knightBonus10) * 10
                                +   popCount (whites .&. knightBonus5) * 5
                                -   popCount (blacks .&. invert knightBonus5) * 5
                                -   popCount (blacks .&. invert knightBonus10) * 10
                                -   popCount (blacks .&. invert knightBonus15) * 15
                                -   popCount (blacks .&. invert knightBonus20 .|. whites .&. knightPenalty20) * 20
                                -   popCount (whites .&. knightPenalty30) * 30
                                -   popCount (whites .&. knightPenalty40) * 40
                                -   popCount (whites .&. knightPenalty50) * 50
bishopPosition whites blacks    =   popCount (blacks .&. invert bishopPenalty20) * 20
                                +   popCount (whites .&. bishopBonus10 .|. blacks .&. invert bishopPenalty10) * 10
                                +   popCount (whites .&. bishopBonus5) * 5
                                -   popCount (blacks .&. invert bishopBonus5) * 5
                                -   popCount (whites .&. bishopPenalty10 .|. blacks .&. invert bishopBonus10) * 10
                                -   popCount (whites .&. bishopPenalty20) * 20
rookPosition whites blacks  =   popCount (whites .&. rookBonus10) * 10
                            +   popCount (whites .&. rookBonus5 .|. blacks .&. invert rookPenalty5) * 5
                            -   popCount (blacks .&. invert rookBonus5 .|. whites .&. rookPenalty5) * 5
                            -   popCount (blacks .&. invert rookBonus10) * 10
queenPosition whites blacks =   popCount (blacks .&. invert queenPenalty20) * 20
                            +   popCount (blacks .&. invert queenPenalty10) * 10
                            +   popCount (whites .&. queenBonus5 .|. blacks .&. invert queenPenalty5) * 5
                            -   popCount (blacks .&. invert queenBonus5 .|. whites .&. queenPenalty5) * 5
                            -   popCount (whites .&. queenPenalty10) * 10
                            -   popCount (whites .&. queenPenalty20) * 20
kingPosition whites blacks  =   popCount (blacks .&. invert kingPenalty50) * 50
                            +   popCount (blacks .&. invert kingPenalty40) * 40
                            +   popCount (whites .&. kingBonus30 .|. blacks .&. invert kingPenalty30) * 30
                            +   popCount (whites .&. kingBonus20 .|. blacks .&. invert kingPenalty20) * 20
                            +   popCount (whites .&. kingBonus10 .|. blacks .&. invert kingPenalty10) * 10
                            -   popCount (blacks .&. invert kingBonus10 .|. whites .&. kingPenalty10) * 10
                            -   popCount (blacks .&. invert kingBonus20 .|. whites .&. kingPenalty20) * 20
                            -   popCount (blacks .&. invert kingBonus30 .|. whites .&. kingPenalty30) * 30
                            -   popCount (whites .&. kingPenalty40) * 40
                            -   popCount (whites .&. kingPenalty50) * 50
kingEPosition whites blacks =   popCount (blacks .&. invert kingEPenalty50) * 50
                            +   popCount (whites .&. kingEBonus40 .|. blacks .&. invert kingEPenalty40) * 40
                            +   popCount (whites .&. kingEBonus30 .|. blacks .&. invert kingEPenalty30) * 30
                            +   popCount (whites .&. kingEBonus20 .|. blacks .&. invert kingEPenalty20) * 20
                            +   popCount (blacks .&. invert kingEPenalty10) * 10
                            -   popCount (whites .&. kingEPenalty10) * 10
                            -   popCount (blacks .&. invert kingEBonus20 .|. whites .&. kingEPenalty20) * 20
                            -   popCount (blacks .&. invert kingEBonus30 .|. whites .&. kingEPenalty30) *30
                            -   popCount (blacks .&. invert kingEBonus40 .|. whites .&. kingEPenalty40) *40
                            -   popCount (whites .&. kingEPenalty50) * 50

pawnBonus50,pawnBonus30,pawnBonus25,pawnBonus20,pawnBonus10,pawnBonus5 :: BitBoard
pawnPenalty5,pawnPenalty10,pawnPenalty20 :: BitBoard
knightBonus20,knightBonus15,knightBonus10,knightBonus5 :: BitBoard
knightPenalty20,knightPenalty30,knightPenalty40,knightPenalty50 :: BitBoard
bishopBonus10,bishopBonus5,bishopPenalty10,bishopPenalty20 :: BitBoard
rookBonus10,rookBonus5,rookPenalty5 :: BitBoard
queenBonus5,queenPenalty5,queenPenalty10,queenPenalty20 :: BitBoard
kingBonus30,kingBonus20,kingBonus10,kingPenalty10,kingPenalty20,kingPenalty30,kingPenalty40,kingPenalty50 :: BitBoard
kingEBonus40,kingEBonus30,kingEBonus20,kingEPenalty10,kingEPenalty20,kingEPenalty30,kingEPenalty40,kingEPenalty50 :: BitBoard
pawnBonus50     = 0x00ff000000000000
pawnBonus30     = 0x0000180000000000
pawnBonus25     = 0x0000001800000000
pawnBonus20     = 0x0000240018000000
pawnBonus10     = 0x0000c32400006600
pawnBonus5      = 0x000000c300818100
pawnPenalty5    = 0x0000000000420000
pawnPenalty10   = 0x0000000000240000
pawnPenalty20   = 0x0000000000001800
knightBonus20   = 0x0000001818000000
knightBonus15   = 0x0000182424180000
knightBonus10   = 0x0000240000240000
knightBonus5    = 0x0000004200421800
knightPenalty20 = 0x0042000000004200
knightPenalty30 = 0x3c0081818181003c
knightPenalty40 = 0x4281000000008142
knightPenalty50 = 0x8100000000000081
bishopBonus10   = 0x000018183c7e0000
bishopBonus5    = 0x0000246600004200
bishopPenalty10 = 0x7e8181818181817e
bishopPenalty20 = 0x8100000000000081
rookBonus10     = 0x007e000000000000
rookBonus5      = 0x8100000000000018
rookPenalty5    = 0x0000818181818100
queenBonus5     = 0x00003c3c3c7c2000
queenPenalty5   = 0x1800810100000018
queenPenalty10  = 0x6681810000818166
queenPenalty20  = 0x8100000000000081
kingBonus30     = 0x0000000000000042
kingBonus20     = 0x000000000000c381
kingBonus10     = 0x0000000000000024
kingPenalty10   = 0x0000000000810000
kingPenalty20   = 0x00000000817e0000
kingPenalty30   = 0x8181818166000000
kingPenalty40   = 0x6666666618000000
kingPenalty50   = 0x1818181800000000
kingEBonus40    = 0x0000001818000000
kingEBonus30    = 0x0000184242180000
kingEBonus20    = 0x0000240000240000
kingEPenalty10  = 0x0024424242420000
kingEPenalty20  = 0x1842000000000000
kingEPenalty30  = 0x248181818181c27e
kingEPenalty40  = 0x4200000000000000
kingEPenalty50  = 0x8100000000000000
