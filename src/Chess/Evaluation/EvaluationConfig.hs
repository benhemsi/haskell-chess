module Chess.Evaluation.EvaluationConfig where

import Chess.OpeningTable.OpeningTableSettings (OpeningTableSettings)
import Chess.Piece
import Control.Lens

data EvaluationConfig =
  EvaluationConfig
    { openingTableSettings :: OpeningTableSettings
    , pieceWeightings :: PieceWeightings
    }

data PieceWeightings =
  PieceWeightings
    { pawnWeight, knightWeight, bishopWeight, rookWeight, queenWeight :: Double
    }

calculatePieceWeight :: PieceWeightings -> PieceType -> Double
calculatePieceWeight pieceWeights piece = weight
  where
    weight =
      case piece of
        Pawn -> pawnWeight pieceWeights
        Knight -> knightWeight pieceWeights
        Bishop -> bishopWeight pieceWeights
        Rook -> rookWeight pieceWeights
        Queen -> queenWeight pieceWeights

calculatePieceWeightings :: PieceWeightings -> [PieceType] -> Double
calculatePieceWeightings weightings = sum . map (calculatePieceWeight weightings)