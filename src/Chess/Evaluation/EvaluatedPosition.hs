{-# LANGUAGE TemplateHaskell #-}

module Chess.Evaluation.EvaluatedPosition where

import Chess.Board
import Chess.Evaluation.Evaluation
import Chess.Evaluation.EvaluationBoard
import Chess.Evaluation.GamePhase
import Chess.Fen
import Chess.Move
import Chess.Piece
import Chess.Position
import Control.Lens
import Data.Array
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Chess.Moves.MoveFiltering
import Piece.King

data EvaluatedPosition =
  EvaluatedPosition
    { _position :: Position
    , _moves :: [MoveTypes]
    , _evaluation :: Evaluation
    }

makeLenses ''EvaluatedPosition
-- getSquareAttackers :: Map.Map PieceOnSquare [MoveTypes] -> Map.Map Square PieceList
-- getSquareAttackers piecesWithMoves = grouped
--   where
--     exploded = do
--       (pce, mvs) <- Map.toList piecesWithMoves
--       Just attackedSq <- map attackedSquare mvs
--       return (attackedSq, [pce])
--     grouped = Map.fromListWith (++) exploded
-- getChess.EvaluationBoard :: Position -> Chess.EvaluationBoard
-- getChess.EvaluationBoard pos = evaluationBoard -- TODO fill in moves field inc castling moves
--   where
--     likeMoves = getValidMoves (view (fen . nextToMove) pos) pos
--     oppoMoves = getValidMoves (oppoColour (view (fen . nextToMove) pos)) pos
--     whiteSquareAttackers = getSquareAttackers likeMoves
--     blackSquareAttackers = getSquareAttackers oppoMoves
--     evaluationSquares = do
--       i <- range (minBound, maxBound)
--       let whitePL = Map.findWithDefault [] i whiteSquareAttackers
--           blackPL = Map.findWithDefault [] i blackSquareAttackers
--       return (i, Chess.EvaluationSquare whitePL blackPL)
--     evaluationBoard = array (minBound, maxBound) evaluationSquares
-- class Evaluable p where
--   offensiveChess.Evaluation :: p -> PieceColour -> Square -> FullPieceList -> Chess.EvaluationBoard -> Chess.Evaluation
--   defensiveChess.Evaluation :: p -> PieceColour -> Square -> FullPieceList -> Chess.EvaluationBoard -> Chess.Evaluation
--   offensiveChess.EvaluationWeighting :: p -> GamePhase -> Double
--   defensiveChess.EvaluationWeighting :: p -> GamePhase -> Double
--   evaluate :: p -> PieceColour -> Square -> FullPieceList -> Chess.EvaluationBoard -> GamePhase -> Chess.Evaluation
--   evaluate pce colour sq fullPL evalBoard gamePhase = output
--     where
--       offence = offensiveChess.Evaluation pce colour sq fullPL evalBoard
--       defence = defensiveChess.Evaluation pce colour sq fullPL evalBoard
--       offenceChess.Evaluation = offensiveChess.EvaluationWeighting pce gamePhase * view positionChess.Evaluation offence
--       defenceChess.Evaluation = defensiveChess.EvaluationWeighting pce gamePhase * view positionChess.Evaluation defence
--       output = Chess.Evaluation (offenceChess.Evaluation - defenceChess.Evaluation) colour
-- instance Evaluable King where
--   defensiveChess.EvaluationWeighting _ gamePhase =
--     case gamePhase of
--       Opening -> 10
--       MiddleGame -> 10
--       EndGame -> 0
--   offensiveChess.EvaluationWeighting _ gamePhase =
--     case gamePhase of
--       Opening -> 0
--       MiddleGame -> 0
--       EndGame -> 10
--   defensiveChess.Evaluation _ colour sq _ evalBoard = Chess.Evaluation (fromIntegral countOfNearbyAttackers) colour
--     where
--       squaresToCheck = mapMaybe attackedSquare (flattenMoves (emptyBoardMoves K sq))
--       evaluationSquares = map (evalBoard !) squaresToCheck
--       countOfNearbyAttackers = sum $ map (countAttackers colour) evaluationSquares
--   offensiveChess.Evaluation _ _ _ _ _ = mempty
-- evaluatePosition :: Position -> Chess.Evaluation
-- evaluatePosition pos = output
--   where
--     evalBoard = getChess.EvaluationBoard pos
--     output = mempty
