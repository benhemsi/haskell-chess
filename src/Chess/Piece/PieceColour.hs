{-# LANGUAGE DeriveGeneric #-}

module Chess.Piece.PieceColour where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Text.Read

data PieceColour
  = White
  | Black
  deriving (Eq, Enum, Generic)

instance ToJSON PieceColour

instance FromJSON PieceColour

instance Show PieceColour where
  show White = "w"
  show Black = "b"

instance Read PieceColour where
  readPrec = do
    Ident s <- lexP
    case s of
      "w" -> return White
      "b" -> return Black
      _ -> pfail
  readListPrec = readListPrecDefault
  readList = readListDefault

oppoColour :: PieceColour -> PieceColour
oppoColour White = Black
oppoColour Black = White

instance Arbitrary PieceColour where
  arbitrary = chooseEnum (White, Black)
