{-# LANGUAGE DeriveGeneric #-}

module Chess.Evaluation.PieceWeightings where

import Chess.Fen
import Chess.OpeningTable.OpeningTableSettings (OpeningTableSettings)
import Chess.Piece
import Control.Lens
import Data.Aeson (FromJSON)
import qualified Data.Map as Map
import GHC.Generics (Generic)

data PieceWeightings =
  PieceWeightings
    { pawnWeight, knightWeight, bishopWeight, rookWeight, queenWeight :: Double
    }
  deriving (Generic, Show)

instance FromJSON PieceWeightings

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

calculateFenPieceWeightings :: PieceWeightings -> FenRepresentation -> Double
calculateFenPieceWeightings weightings fen = whiteEval - blackEval
  where
    whitePl = Map.elems (fen ^. pieces . whitePieces)
    blackPl = Map.elems (fen ^. pieces . blackPieces)
    whiteEval = calculatePieceWeightings weightings whitePl
    blackEval = calculatePieceWeightings weightings blackPl
