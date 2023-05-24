{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Chess.OpeningTable.OpeningTable where

import Chess.Board.Square
import Chess.Fen.CastlingPrivileges
import Chess.Piece.PieceColour
import Control.Lens
import qualified Database.Persist.TH as PTH
import Text.Read

PTH.share
  [PTH.mkPersist PTH.sqlSettings {PTH.mpsGenerateLenses = True}, PTH.mkMigrate "migrateAll"]
  [PTH.persistLowerCase|
  OpeningPosition sql=opening_table
    pieceList String
    nextToMove PieceColour
    whiteKingSideCastle Bool
    whiteQueenSideCastle Bool
    blackKingSideCastle Bool
    blackQueenSideCastle Bool
    enPassent EnPassentSquare
    evaluation Double
    Primary pieceList nextToMove whiteKingSideCastle whiteQueenSideCastle blackKingSideCastle blackQueenSideCastle enPassent
|]

type EnPassentSquare = Maybe Square

instance {-# OVERLAPPING #-} Show EnPassentSquare where
  show (Just sq) = show sq
  show Nothing = "-"

instance {-# OVERLAPPING #-} Read EnPassentSquare where
  readPrec = do
    Ident s <- lexP
    if s == "-"
      then return Nothing
      else case readMaybe s of
             Nothing -> pfail
             sq -> return sq

PTH.derivePersistField "EnPassentSquare"

openingPositionCastlingPrivileges :: Lens' OpeningPosition CastlingPrivileges
openingPositionCastlingPrivileges cp2Fcp openingPos =
  let cp =
        CastlingPrivileges
          (openingPos ^. openingPositionWhiteKingSideCastle)
          (openingPos ^. openingPositionWhiteQueenSideCastle)
          (openingPos ^. openingPositionBlackKingSideCastle)
          (openingPos ^. openingPositionBlackQueenSideCastle)
      setF (CastlingPrivileges a b c d) =
        foldr
          (\(lens, value) curr -> set lens value curr)
          openingPos
          [ (openingPositionWhiteKingSideCastle, a)
          , (openingPositionWhiteQueenSideCastle, b)
          , (openingPositionBlackKingSideCastle, c)
          , (openingPositionBlackQueenSideCastle, d)
          ]
   in setF <$> cp2Fcp cp
