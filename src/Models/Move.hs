{-# LANGUAGE TemplateHaskell #-}

module Models.Move where

import Control.Lens
import Models.Board

data Move =
  Move
    { _start, _end :: Square
    }
  deriving (Eq)

makeLenses ''Move

data MoveTypes
  = Mv Move
  | EnP EnPassent
  | PP PawnPromotion
  | Cst Castle
  deriving (Eq)

instance Show MoveTypes where
  show mv = show (startingSquare mv) ++ "->" ++ end
    where
      end = maybe "" show (attackedSquare mv)

data EnPassent =
  EnPassent Move Square
  deriving (Eq)

newtype PawnPromotion =
  PawnPromotion Move
  deriving (Eq)

data Castle =
  Castle Move Move
  deriving (Eq)

data SlidingMoves =
  SlidingMoves [Move] [Move] [Move] [Move]
  deriving (Eq)

data PawnMoves =
  PM
    { forward, jump :: Maybe Move
    , takes :: [Move]
    , enPassent :: [EnPassent]
    , promotion :: Maybe PawnPromotion
    , promotionTakes :: [PawnPromotion]
    }
  deriving (Eq)

data KingMoves =
  KM [Move] (Maybe Castle) (Maybe Castle)
  deriving (Eq)

data Moves
  = Moves [Move]
  | Sliders SlidingMoves
  | QueenMoves
      { bishopMoves :: SlidingMoves
      , rookMoves :: SlidingMoves
      }
  | PawnMoves
      { white, black :: PawnMoves
      }
  | KingMoves KingMoves
  deriving (Eq)

class Moveable p where
  emptyBoardMoves :: p -> Square -> Moves

startingSquare :: MoveTypes -> Square
startingSquare (Mv mv) = _start mv
startingSquare (EnP (EnPassent mv _)) = _start mv
startingSquare (PP (PawnPromotion mv)) = _start mv
startingSquare (Cst (Castle mv _)) = _start mv

attackedSquare :: MoveTypes -> Maybe Square
attackedSquare (Mv mv) = Just (_end mv)
attackedSquare (EnP (EnPassent mv _)) = Just (_end mv)
attackedSquare (PP (PawnPromotion mv)) = Just (_end mv)
attackedSquare (Cst _) = Nothing
