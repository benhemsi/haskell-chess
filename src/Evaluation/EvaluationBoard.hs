module Evaluation.EvaluationBoard where

import Data.Array
import Data.List (groupBy)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Models.Board
import Models.Move
import Models.PieceColour
import Models.PieceList
import Models.Square
import Models.Position
import Moves.MoveLogic

data EvaluationSquare = EvaluationSquare {_whiteAttackers, _blackAttackers :: PieceList}

type EvaluationBoard = Board EvaluationSquare


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
