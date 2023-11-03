{-# LANGUAGE DeriveGeneric #-}

module Chess.Evaluation.MinAndMaxEval where

import Chess.Fen
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

data MinAndMaxEval =
  MinAndMaxEval
    { minFen, maxFen :: (Double, FenRepresentation)
    }
  deriving (Show, Generic)

instance ToJSON MinAndMaxEval

instance FromJSON MinAndMaxEval
