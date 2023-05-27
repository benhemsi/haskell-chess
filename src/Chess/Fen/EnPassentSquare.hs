{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Chess.Fen.EnPassentSquare where

import Chess.Board
import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import Database.Persist as PS
import Database.Persist.Sql
import GHC.Generics
import Test.QuickCheck (Arbitrary(arbitrary))
import Text.Read

newtype EnPassentSquare =
  EnPSq
    { _enPassentSq :: Maybe Square
    }
  deriving (Eq, Generic, Ord)

makeLenses ''EnPassentSquare

instance Show EnPassentSquare where
  show (EnPSq (Just sq)) = show sq
  show (EnPSq Nothing) = "-"

instance Read EnPassentSquare where
  readPrec =
    (do Symbol "-" <- lexP
        return $ EnPSq Nothing) +++
    (do Ident s <- lexP
        case readMaybe s of
          Nothing -> pfail
          sq -> return $ EnPSq sq)

instance ToJSON EnPassentSquare

instance FromJSON EnPassentSquare

instance PersistField EnPassentSquare where
  toPersistValue (EnPSq (Just sq)) = PersistInt64 (fromIntegral $ fromEnum sq)
  toPersistValue (EnPSq Nothing) = PersistNull
  fromPersistValue (PersistInt64 n) = Right (EnPSq $ Just $ toEnum $ fromIntegral n)
  fromPersistValue PersistNull = Right (EnPSq Nothing)
  fromPersistValue x =
    Left $
    "Error when trying to deserialize a EnPassentSquare: expected PersistInt64 or PersistNull, received: " <>
    T.pack (show x)

instance PersistFieldSql EnPassentSquare where
  sqlType _ = SqlInt32

instance Arbitrary EnPassentSquare where
  arbitrary = do
    enPSq <- arbitrary
    return $ EnPSq enPSq
