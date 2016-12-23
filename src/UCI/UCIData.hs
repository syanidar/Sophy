module UCIData where

import BasicData

type Name               = String
type Value              = String
type Author             = String
type Code               = Int

data UCICommand     = UCI
                    | Debug Bool
                    | IsReady
                    | SetOption Name (Maybe Value)
                    | RegisterLater
                    | Register Name Code
                    | UCINewGame
                    | InPosition Position
                    | Go [GoToken]
                    | Stop
                    | PonderHit
                    | Quit
    deriving(Show,Eq)

data GoToken        = SearchMoves [String]
                    | Ponder
                    | WTime Int
                    | BTime Int
                    | WInc Int
                    | BInc Int
                    | MovesToGO Int
                    | DepthLimit Int
                    | NodeLimit Int
                    | Mate Int
                    | MoveTime Int
                    | Infinite
    deriving(Show,Eq)
