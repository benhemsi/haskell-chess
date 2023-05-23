{-# LANGUAGE TemplateHaskell #-}

module Evaluation.EvaluationBoard where

import Control.Lens
import Chess.Board
import Chess.Piece

data EvaluationSquare =
  EvaluationSquare
    { _whiteAttackers, _blackAttackers :: PieceList
    }

makeLenses ''EvaluationSquare

type EvaluationBoard = Board EvaluationSquare
-- getAttackers :: PieceColour -> EvaluationSquare -> PieceList
-- getAttackers colour =
--   case colour of
--     White -> view whiteAttackers
--     Black -> view blackAttackers
-- getDefenders :: PieceColour -> EvaluationSquare -> PieceList
-- getDefenders colour = getAttackers (oppoColour colour)
-- countAttackers :: PieceColour -> EvaluationSquare -> Int
-- countAttackers colour sq = length $ getAttackers colour sq
-- countDefenders :: PieceColour -> EvaluationSquare -> Int
-- countDefenders colour sq = length $ getDefenders colour sq
-- netAttackerCount :: PieceColour -> EvaluationSquare -> Int
-- netAttackerCount colour sq = countAttackers colour sq - countDefenders colour sq
