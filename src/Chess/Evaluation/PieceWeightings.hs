{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Chess.Evaluation.PieceWeightings where

import Barbies
import Chess.Fen
import Chess.OpeningTable.OpeningTableSettings (OpeningTableSettings)
import Chess.Piece
import Control.Lens
import Data.Aeson (FromJSON, parseJSON)
import qualified Data.Map as Map
import GHC.Generics (Generic)

data PieceWeightings_ f =
  PieceWeightings
    { pawnWeight, knightWeight, bishopWeight, rookWeight, queenWeight :: f Double
    }
  deriving (Generic, FunctorB, TraversableB, ApplicativeB, ConstraintsB)

type PieceWeightings = PieceWeightings_ Identity

deriving instance
         AllBF Show f PieceWeightings_ => Show (PieceWeightings_ f)

instance FromJSON (PieceWeightings_ Maybe)

instance FromJSON PieceWeightings where
  parseJSON = fmap (pieceWeightingsWithDefault defaultPieceWeightings) . parseJSON

defaultPieceWeightings :: PieceWeightings
defaultPieceWeightings = PieceWeightings 1 3 3 5 9

pieceWeightingsWithDefault :: PieceWeightings -> PieceWeightings_ Maybe -> PieceWeightings
pieceWeightingsWithDefault = bzipWith (`maybe` pure)

calculatePieceWeight :: PieceWeightings -> PieceType -> Double
calculatePieceWeight pieceWeights piece = weight
  where
    Identity weight =
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
