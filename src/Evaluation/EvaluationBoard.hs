module Evaluation.EvaluationBoard where

import Data.Array
import Data.List (groupBy)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Models.Board
import Models.FullPieceList
import Models.Move
import Models.PieceColour
import Models.PieceList
import Models.PieceOnSquare
import Models.Square
import Models.Position
import Moves.MoveLogic

data EvaluationSquare = EvaluationSquare {_whiteAttackers, _blackAttackers :: PieceList}

type EvaluationBoard = Board EvaluationSquare

getSquareAttackers :: PieceColour -> Position -> Map.Map Square [PieceOnSquare]
getSquareAttackers col pos = grouped
  where
    piecesWithMoves = getValidMoves col pos
    exploded = do
      (piece, moves) <- piecesWithMoves
      Just attackedSq <- map attackedSquare moves
      return (attackedSq, [piece])
    grouped = Map.fromListWith (++) exploded

buildEvaluationBoard :: Position -> EvaluationBoard
buildEvaluationBoard pos = array (minBound, maxBound) output
  where
    whiteSquareAttackers = getSquareAttackers White pos
    blackSquareAttackers = getSquareAttackers Black pos
    output = do
      i <- range (minBound, maxBound)
      let whitePL = Map.findWithDefault [] i whiteSquareAttackers
          blackPL = Map.findWithDefault [] i blackSquareAttackers
      return (i, EvaluationSquare whitePL blackPL)

getAttackers :: Square -> PieceColour -> EvaluationBoard -> PieceList
getAttackers sq colour = getPieceList . (! sq)
  where
    getPieceList = case colour of
      White -> _whiteAttackers
      Black -> _blackAttackers

checkIfSquareAttacked :: Square -> PieceColour -> EvaluationBoard -> Bool
checkIfSquareAttacked sq colour = not . null . getAttackers sq colour

getNumberOfAttackers :: Square -> PieceColour -> EvaluationBoard -> Int
getNumberOfAttackers sq colour = length . getAttackers sq colour
