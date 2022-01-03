module Models.EvaluationBoard where

import Models.PieceList

data EvaluationSquare = EvaluationSquare {whiteAttackers, blackAttackers :: PieceList}
