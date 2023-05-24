{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Chess.Piece.PieceColour where

import Data.Aeson
import qualified Data.Text as T
import Database.Persist.Class.PersistField
import Database.Persist.PersistValue
import Database.Persist.Sql
import GHC.Generics
import Test.QuickCheck
import Text.Read

data PieceColour
  = White
  | Black
  deriving (Eq, Enum, Generic, Ord)

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

instance PersistField PieceColour where
  toPersistValue White = PersistBool False
  toPersistValue Black = PersistBool True
  fromPersistValue (PersistBool False) = Right White
  fromPersistValue (PersistBool True) = Right Black
  fromPersistValue x =
    Left $ "Error when trying to deserialize a PieceColour: expected PersistBool, received: " <> T.pack (show x)

instance PersistFieldSql PieceColour where
  sqlType _ = SqlBool
