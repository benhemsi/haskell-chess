{-# LANGUAGE TemplateHaskell #-}

module Evaluation.EvaluatedPosition where

import Data.Array
import Control.Lens
import Control.Monad.State
import Evaluation.Evaluation
import Evaluation.EvaluationBoard
import Models.PieceColour
import Models.Position
import Models.Move
import qualified Data.Map as Map
import Models.Square
import Models.PieceList
import Moves.MoveLogic
import Models.FenRepresentation
import Models.PieceOnSquare

data EvaluatedPosition = EvaluatedPosition {_position :: Position, _moves :: [MoveTypes], _evaluation :: Evaluation}

makeLenses ''EvaluatedPosition

getSquareAttackers :: [(PieceOnSquare, [MoveTypes])] -> Map.Map Square PieceList
getSquareAttackers moves = grouped
  where
    exploded = do
      (piece, moves) <- moves
      Just attackedSq <- map attackedSquare moves
      return (attackedSq, [piece])
    grouped = Map.fromListWith (++) exploded

evaluatePosition :: Position -> EvaluatedPosition
evaluatePosition pos = EvaluatedPosition pos [] (Evaluation 0 evaluationBoard) -- TODO fill in moves field inc castling moves
  where
    likeMoves = getValidMoves (view (fen . nextToMove) pos) pos
    oppoMoves = getValidMoves (oppoColour (view (fen . nextToMove) pos)) pos
    whiteSquareAttackers = getSquareAttackers likeMoves
    blackSquareAttackers = getSquareAttackers oppoMoves
    evaluationSquares = do
      i <- range (minBound, maxBound)
      let whitePL = Map.findWithDefault [] i whiteSquareAttackers
          blackPL = Map.findWithDefault [] i blackSquareAttackers
      return (i, EvaluationSquare whitePL blackPL)
    evaluationBoard = array (minBound, maxBound) evaluationSquares
