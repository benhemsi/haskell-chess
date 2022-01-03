module Models.EvaluationBoard where

import Models.PieceOnSquare (PieceList)

data EvaluationSquare = EvaluationSquare {whiteAttackers, blackAttackers :: PieceList}


