{-# LANGUAGE TemplateHaskell #-}

module Evaluation.EvaluatedPosition where

import Control.Lens
import Data.Array
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Evaluation.Evaluation
import Evaluation.EvaluationBoard
import Evaluation.GamePhase
import Models.Board
import Models.FenRepresentation
import Models.FullPieceList
import Models.Move
import Models.PieceColour
import Models.PieceList
import Models.PieceOnSquare
import Models.Position
import Moves.MoveFiltering
import Piece.King

data EvaluatedPosition =
  EvaluatedPosition
    { _position :: Position
    , _moves :: [MoveTypes]
    , _evaluation :: Evaluation
    }

makeLenses ''EvaluatedPosition

getSquareAttackers :: Map.Map PieceOnSquare [MoveTypes] -> Map.Map Square PieceList
getSquareAttackers piecesWithMoves = grouped
  where
    exploded = do
      (pce, mvs) <- Map.toList piecesWithMoves
      Just attackedSq <- map attackedSquare mvs
      return (attackedSq, [pce])
    grouped = Map.fromListWith (++) exploded

getEvaluationBoard :: Position -> EvaluationBoard
getEvaluationBoard pos = evaluationBoard -- TODO fill in moves field inc castling moves
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

class Evaluable p where
  offensiveEvaluation :: p -> PieceColour -> Square -> FullPieceList -> EvaluationBoard -> Evaluation
  defensiveEvaluation :: p -> PieceColour -> Square -> FullPieceList -> EvaluationBoard -> Evaluation
  offensiveEvaluationWeighting :: p -> GamePhase -> Double
  defensiveEvaluationWeighting :: p -> GamePhase -> Double
  evaluate :: p -> PieceColour -> Square -> FullPieceList -> EvaluationBoard -> GamePhase -> Evaluation
  evaluate pce colour sq fullPL evalBoard gamePhase = output
    where
      offence = offensiveEvaluation pce colour sq fullPL evalBoard
      defence = defensiveEvaluation pce colour sq fullPL evalBoard
      offenceEvaluation = offensiveEvaluationWeighting pce gamePhase * view positionEvaluation offence
      defenceEvaluation = defensiveEvaluationWeighting pce gamePhase * view positionEvaluation defence
      output = Evaluation (offenceEvaluation - defenceEvaluation) colour

instance Evaluable King where
  defensiveEvaluationWeighting _ gamePhase =
    case gamePhase of
      Opening -> 10
      MiddleGame -> 10
      EndGame -> 0
  offensiveEvaluationWeighting _ gamePhase =
    case gamePhase of
      Opening -> 0
      MiddleGame -> 0
      EndGame -> 10
  defensiveEvaluation _ colour sq _ evalBoard = Evaluation (fromIntegral countOfNearbyAttackers) colour
    where
      squaresToCheck = mapMaybe attackedSquare (flattenMoves (emptyBoardMoves K sq))
      evaluationSquares = map (evalBoard !) squaresToCheck
      countOfNearbyAttackers = sum $ map (countAttackers colour) evaluationSquares
  offensiveEvaluation _ _ _ _ _ = mempty

evaluatePosition :: Position -> Evaluation
evaluatePosition pos = output
  where
    evalBoard = getEvaluationBoard pos
    output = mempty
