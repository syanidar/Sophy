{-# LANGUAGE Strict #-}
module AttackTable where

import Alias
import BasicConstant
import BasicData
import Bitwise
import qualified BitBoard as B
import Data.Bits
import qualified Data.Vector.Unboxed as U
import qualified Data.Array.Unboxed as A
import qualified Data.Vector as V
import Control.Applicative
import Data.Maybe

{-# INLINE pieceAttack #-}
pieceAttack :: PieceType -> Occupancy -> Square -> BitBoard
pieceAttack Knight _    = knightAttack
pieceAttack Bishop o    = bishopAttack o
pieceAttack Rook o      = rookAttack o
pieceAttack Queen o     = queenAttack o
pieceAttack King _      = kingAttack

{-# INLINE pawnAttack #-}
pawnAttack :: Color -> Square -> BitBoard
pawnAttack White = (whitePawnAttackTable U.!)
pawnAttack Black = (blackPawnAttackTable U.!)

whitePawnAttackTable,blackPawnAttackTable :: U.Vector BitBoard
whitePawnAttackTable = U.fromList.map (B.pawnAttack White .bit) $ [0 .. 63]
blackPawnAttackTable = U.fromList.map (B.pawnAttack Black .bit) $ [0 .. 63]

{-# INLINE knightAttack #-}
{-# INLINE kingAttack #-}
knightAttack,kingAttack :: Square -> BitBoard
knightAttack = (knightAttackTable U.!)
kingAttack = (kingAttackTable U.!)

knightAttackTable,kingAttackTable :: U.Vector BitBoard
knightAttackTable = U.fromList.map (B.knightAttack.bit) $ [0 .. 63]
kingAttackTable = U.fromList.map (B.kingAttack.bit) $ [0 .. 63]

{-# INLINE bishopAttack #-}
{-# INLINE rookAttack #-}
{-# INLINE queenAttack #-}
bishopAttack,rookAttack,queenAttack :: Occupancy -> Square -> BitBoard
bishopAttack occupancy square = (bishopAttackTable V.! square) A.! ix
    where   relOcc      = bishopRelevantOccupancyTable U.! square
            magic       = bishopMagicTable U.! square
            distance    = bishopShiftTable U.! square
            ix          = fromIntegral $ ((occupancy .&. relOcc) * magic) `shiftR` distance
rookAttack occupancy square = (rookAttackTable V.! square) A.! ix
    where   relOcc      = rookRelevantOccupancyTable U.! square
            magic       = rookMagicTable U.! square
            distance    = rookShiftTable U.! square
            ix          = fromIntegral $ ((occupancy .&. relOcc) * magic) `shiftR` distance
queenAttack occupancy square    =   bishopAttack occupancy square
                                .|. rookAttack occupancy square

bishopAttackTable,rookAttackTable :: V.Vector (A.UArray Int BitBoard)
bishopAttackTable = V.fromList.map table $ [0 .. 63]
    where   tableSize square = 1 `shiftL` (64 - bishopShiftTable U.! square)
            table :: Square -> A.UArray Int BitBoard
            table square = A.array (0, tableSize square - 1) $ do
                occupancy <- scanOccupancy $ bishopRelevantOccupancyTable U.! square
                let ix = fromIntegral $ ((bishopMagicTable U.! square) * occupancy) `shiftR` bishopShiftTable U.! square
                    at = B.bishopAttack occupancy (bit square)
                return (ix, at)
rookAttackTable = V.fromList.map table $ [0 .. 63]
    where   tableSize square = 1 `shiftL`(64 - rookShiftTable U.! square)
            table :: Square -> A.UArray Int BitBoard
            table square = A.array (0, tableSize square - 1) $ do
                occupancy <- scanOccupancy $ rookRelevantOccupancyTable U.! square
                let ix = fromIntegral $ ((rookMagicTable U.! square) * occupancy) `shiftR` rookShiftTable U.! square
                    at = B.rookAttack occupancy (bit square)
                return (ix, at)

bishopShiftTable,rookShiftTable :: U.Vector Int
bishopShiftTable    = U.fromList $ zipWith ($) fs bishopShift
    where   fs = map (maybe id (const (+1))) bishopBetterMagics
rookShiftTable      = U.fromList $ zipWith ($) fs rookShift
    where   fs = map (maybe id (const (+1))) rookBetterMagics

bishopShift,rookShift :: [Int]
bishopShift = U.toList.U.map ((64 -).popCount) $ bishopRelevantOccupancyTable
rookShift   = U.toList.U.map ((64 -).popCount) $ rookRelevantOccupancyTable

bishopMagicTable,rookMagicTable :: U.Vector Magic
bishopMagicTable    = U.fromList.map fromJust $ zipWith (<|>) bishopBetterMagics bishopMagics
rookMagicTable      = U.fromList.map fromJust $ zipWith (<|>) rookBetterMagics rookMagics

bishopMagics,rookMagics,bishopBetterMagics,rookBetterMagics:: [Maybe Magic]
bishopMagics        = [Just 297239860681637905,Just 2306977713872742416,Just 1298347312707559424,Just 3464398447944138752,Just 2346958215743279112,Just 613061312618106913,Just 571771850850368,Just 42257564886435840,Just 3458773928455996424,Just 1152925971444205794,Just 1284234954211344,Just 290703194999750916,Just 729619493319344128,Just 4809855401712951840,Just 4514631320078368,Just 1152957789581352962,Just 22570791943538688,Just 580966688594362500,Just 9225624253314105362,Just 2254005313029256,Just 4828140423966883840,Just 9499782617767936,Just 9241597833942142976,Just 4613974103232611488,Just 4756540147852378624,Just 20409689426891008,Just 452616298472670096,Just 9856022302949504,Just 9232660779822497793,Just 9278400532408569856,Just 20267297868417792,Just 9232521073252263948,Just 22570791943538688,Just 288802277590828290,Just 2306005745529790592,Just 563225930498304,Just 18014673655890176,Just 108649899365566464,Just 2260785975951880,Just 577587279275589956,Just 145341829103616,Just 1225157323144728064,Just 882741347093581824,Just 18437435880899072,Just 4615065925962498564,Just 598151591363840,Just 4757007654428082704,Just 380836902200156672,Just 571771850850368,Just 14123858002113462280,Just 72343759186296849,Just 4613937831671758848,Just 18084836141047809,Just 18084836141047809,Just 297239860681637905,Just 2306977713872742416,Just 42257564886435840,Just 1152957789581352962,Just 75110933598336,Just 1152930335069307904,Just 2260785975951880,Just 17867756093697,Just 3458773928455996424,Just 297239860681637905]
rookMagics          = [Just 36029355902648322,Just 1243011111888887808,Just 36046391495106562,Just 72067489911022084,Just 13871135232960757762,Just 72061992122384392,Just 36031545814819328,Just 13907116478248919178,Just 9223512780788219904,Just 18084786045190216,Just 4620834505463173120,Just 9223512808778371073,Just 144678207155994657,Just 9269815717300012032,Just 327918360753078784,Just 5190820795896578688,Just 324269069354156288,Just 4652218690020839488,Just 4620834505463173120,Just 46303183693170693,Just 9304578118062707713,Just 11529637808356526592,Just 13515196946491912,Just 1315053290233432132,Just 1729593365290295428,Just 9296485172794886272,Just 4612004902570311813,Just 8661284111289680000,Just 9295993706128476704,Just 18438812153676800,Just 327918360753078784,Just 6359084348884451652,Just 13581168171549072,Just 9227880172038916173,Just 36046391495106562,Just 9223512808778371073,Just 4611826790283871232,Just 18438812153676800,Just 18858840703373824,Just 19145815335370852,Just 13853073004723994624,Just 283678840324160,Just 226448828830711840,Just 85621169612423296,Just 9225641463231086612,Just 220957959864254466,Just 1159687968018137089,Just 2305843836599009281,Just 324269069354156288,Just 5779561982977900800,Just 864832003383493248,Just 8661284111289680000,Just 2341915789452443904,Just 10387693295198404736,Just 2310382927361491968,Just 5190820795896578688,Just 9259700049250746626,Just 184811413029028001,Just 216454293870354433,Just 81101112341447937,Just 642114975498255,Just 36591753683535906,Just 9514145652129923604,Just 79448909054218]
bishopBetterMagics  = [Just 18140241445301226495,Just 17958520202983371190,Nothing,Nothing,Nothing,Nothing,Just 8997669494818077369,Just 16066436173231226866,Just 17558112840401141755,Just 13042088030542526463,Nothing,Nothing,Nothing,Nothing,Just 9076995261732224988,Just 13726971237899108351,Just 8340696151287451643,Just 4728811472401641468,Nothing,Nothing,Nothing,Nothing,Just 8938522175157370742,Just 18161331257755361142,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Just 15920182580465156255,Just 17969356424471207979,Nothing,Nothing,Nothing,Nothing,Just 4899804643636939777,Just 5476321683761034753,Just 11526952169365534431,Just 4023960659358743533,Nothing,Nothing,Nothing,Nothing,Just 17834241304901390059,Just 16915510274448739067,Just 6916398712465516023,Just 6916398712465516023,Nothing,Nothing,Nothing,Nothing,Just 17248082727783886655,Just 14735661411988365271]
rookBetterMagics    = [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Just 5260202827150502400,Just 5260202827150502400,Just 5296232809593843200,Just 7007600874156364288,Just 18446743979218685440,Just 18446744030759085568,Nothing,Just 5841168673586058912,Just 17005591892296975654,Just 7061642970158444206,Just 6034823423364870562,Just 1333065189051839990,Just 4692750665688806614,Nothing,Just 1125827562356340,Just 8522499339677771678]

bishopRelevantOccupancyTable,rookRelevantOccupancyTable :: U.Vector Occupancy
bishopRelevantOccupancyTable = U.generate 64 relOcp
    where
        relOcp s = B.bishopAttack 0 (bit s) `without` edge
rookRelevantOccupancyTable = U.generate 64 relOcp
    where
        relOcp s = B.rookAttack 0 (bit s) `without` rookMask s

scanOccupancy :: Occupancy -> [Occupancy]
scanOccupancy o = take size.iterate ((.&. o).subtract 1) $ o
    where size = 1 `shiftL` popCount o

rookMask :: Square -> BitBoard
rookMask = foldl1 (.|.).flip filter [aFile, hFile, eighthRank, firstRank].disjoint.bit
