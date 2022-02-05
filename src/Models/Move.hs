{-# LANGUAGE TemplateHaskell #-}

module Models.Move where

import Control.Lens
import qualified Data.Set as Set
import Models.Square

data Move = Move {_start, _end :: Square}

makeLenses ''Move

data MoveTypes
  = Mv Move
  | EnP EnPassent
  | PP PawnPromotion
  | Cst Castle

data EnPassent = EnPassent Move Square

newtype PawnPromotion = PawnPromotion Move

data Castle = Castle Move Move

data SlidingMoves = SlidingMoves [Move] [Move] [Move] [Move]

data PawnMoves = PM {forward, jump :: Maybe Move, takes :: [Move], enPassent :: [EnPassent], promotion :: Maybe PawnPromotion}

data KingMoves = KM [Move] (Maybe Castle) (Maybe Castle)

data Moves
  = Moves [Move]
  | Sliders SlidingMoves
  | QueenMoves {bishopMoves :: SlidingMoves, rookMoves :: SlidingMoves}
  | PawnMoves {white, black :: PawnMoves}
  | KingMoves KingMoves

class Moveable p where
  emptyBoardMoves :: p -> Square -> Moves

startingSquare :: MoveTypes -> Square
startingSquare (Mv mv) = _start mv
startingSquare (EnP (EnPassent mv _)) = _start mv
startingSquare (PP (PawnPromotion mv)) = _start mv
startingSquare (Cst (Castle mv _)) = _start mv

attackedSquare :: MoveTypes -> Maybe Square
attackedSquare (Mv mv) = Just (_end mv)
attackedSquare (EnP (EnPassent _ sq)) = Just sq
attackedSquare (PP _) = Nothing
attackedSquare (Cst _) = Nothing
