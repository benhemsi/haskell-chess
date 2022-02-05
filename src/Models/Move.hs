module Models.Move where

import qualified Data.Set as Set
import Models.Square

data Move = Move {start, end :: Square}

data EnPassent = EnPassent Move Square

newtype PawnPromotion = PawnPromotion Move

data Castle = Castle Move Move

data SlidingMoves = SlidingMoves [Move] [Move] [Move] [Move]

data PawnMoves = PM {forward, jump, takeLeft, takeRight :: Maybe Move, enPassentLeft, enPassentRight :: Maybe EnPassent, promotion :: Maybe PawnPromotion}

data KingMoves = KM [Move] (Maybe Castle) (Maybe Castle)

data Moves
  = Moves [Move]
  | Sliders SlidingMoves
  | QueenMoves {bishopMoves :: SlidingMoves, rookMoves :: SlidingMoves}
  | PawnMoves {white, black :: PawnMoves}
  | KingMoves KingMoves

class Moveable p where
  emptyBoardMoves :: p -> Square -> Moves

