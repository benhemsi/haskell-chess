{-# LANGUAGE TemplateHaskell #-}

module Models.Move where

import Control.Lens
import Data.Foldable (toList)
import qualified Data.Set as Set
import Models.Square

data Move = Move {_start, _end :: Square} deriving Eq

makeLenses ''Move

data MoveTypes
  = Mv Move
  | EnP EnPassent
  | PP Move
  | Cst Castle deriving Eq

instance Show MoveTypes where
  show mv = show (startingSquare mv) ++ "->" ++ end
    where
      end = maybe "" show (attackedSquare mv)

data EnPassent = EnPassent Move Square deriving Eq

newtype PawnPromotion = PawnPromotion Move deriving Eq

data Castle = Castle Move Move deriving Eq

data SlidingMoves = SlidingMoves [Move] [Move] [Move] [Move] deriving Eq

data PawnMoves = PM {forward, jump :: Maybe Move, takes :: [Move], enPassent :: [EnPassent], promotion :: Maybe PawnPromotion, promotionTakes :: [PawnPromotion]} deriving Eq

data KingMoves = KM [Move] (Maybe Castle) (Maybe Castle) deriving Eq

data Moves
  = Moves [Move]
  | Sliders SlidingMoves
  | QueenMoves {bishopMoves :: SlidingMoves, rookMoves :: SlidingMoves}
  | PawnMoves {white, black :: PawnMoves}
  | KingMoves KingMoves deriving Eq

class Moveable p where
  emptyBoardMoves :: p -> Square -> Moves

startingSquare :: MoveTypes -> Square
startingSquare (Mv mv) = _start mv
startingSquare (EnP (EnPassent mv _)) = _start mv
startingSquare (PP mv) = _start mv
startingSquare (Cst (Castle mv _)) = _start mv

attackedSquare :: MoveTypes -> Maybe Square
attackedSquare (Mv mv) = Just (_end mv)
attackedSquare (EnP (EnPassent _ sq)) = Just sq
attackedSquare (PP mv) = Just (_end mv)
attackedSquare (Cst _) = Nothing

getMoves :: Moves -> [MoveTypes]
getMoves (Moves mvs) = map Mv mvs
getMoves (Sliders (SlidingMoves a b c d)) = map Mv (a ++ b ++ c ++ d)
getMoves (QueenMoves b r) = getMoves (Sliders b) ++ getMoves (Sliders r)
getMoves (KingMoves (KM mvs kc qc)) = map Mv mvs ++ map Cst (toList kc) ++ map Cst (toList qc)
getMoves (PawnMoves w b) = whiteMoves ++ blackMoves
  where
    flattenPawnMoves (PM f j t enP pr prTks) = map Mv (toList f ++ toList j ++ t) ++ map EnP enP ++ map (\(PawnPromotion mv) -> PP mv) (toList pr ++ prTks)
    whiteMoves = flattenPawnMoves w
    blackMoves = flattenPawnMoves b
