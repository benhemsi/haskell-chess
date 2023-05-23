{-# LANGUAGE TemplateHaskell #-}

module Chess.Evaluation.EvaluationBoard where

import Chess.Board
import Chess.Piece
import Control.Lens

data EvaluationSquare =
  EvaluationSquare
    { _whiteAttackers, _blackAttackers :: PieceList
    }

makeLenses ''EvaluationSquare

type EvaluationBoard = Board EvaluationSquare
-- getAttackers :: PieceColour -> Chess.EvaluationSquare -> PieceList
-- getAttackers colour =
--   case colour of
--     White -> view whiteAttackers
--     Black -> view blackAttackers
-- getDefenders :: PieceColour -> Chess.EvaluationSquare -> PieceList
-- getDefenders colour = getAttackers (oppoColour colour)
-- countAttackers :: PieceColour -> Chess.EvaluationSquare -> Int
-- countAttackers colour sq = length $ getAttackers colour sq
-- countDefenders :: PieceColour -> Chess.EvaluationSquare -> Int
-- countDefenders colour sq = length $ getDefenders colour sq
-- netAttackerCount :: PieceColour -> Chess.EvaluationSquare -> Int
-- netAttackerCount colour sq = countAttackers colour sq - countDefenders colour sq
