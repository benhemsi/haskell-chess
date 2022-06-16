{-# LANGUAGE TemplateHaskell #-}

module Evaluation.EvaluationBoard where

import Control.Lens
import Data.Array
import Data.List (groupBy)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Models.Board
import Models.Move
import Models.PieceColour
import Models.PieceList
import Models.Position
import Models.Square
import Moves.MoveLogic

data EvaluationSquare =
  EvaluationSquare
    { _whiteAttackers, _blackAttackers :: PieceList
    }

makeLenses ''EvaluationSquare

type EvaluationBoard = Board EvaluationSquare

getAttackers :: PieceColour -> EvaluationSquare -> PieceList
getAttackers colour =
  case colour of
    White -> view whiteAttackers
    Black -> view blackAttackers

getDefenders :: PieceColour -> EvaluationSquare -> PieceList
getDefenders colour = getAttackers (oppoColour colour)

countAttackers :: PieceColour -> EvaluationSquare -> Int
countAttackers colour sq = length $ getAttackers colour sq

countDefenders :: PieceColour -> EvaluationSquare -> Int
countDefenders colour sq = length $ getDefenders colour sq

netAttackerCount :: PieceColour -> EvaluationSquare -> Int
netAttackerCount colour sq = countAttackers colour sq - countDefenders colour sq
