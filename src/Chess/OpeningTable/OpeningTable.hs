{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Chess.OpeningTable.OpeningTable where

import Chess.Board.Square
import Chess.Fen
import Chess.Piece
import Control.Lens
import Control.Monad.Trans.Reader
import qualified Data.Text as T
import Database.Persist as PS
import Database.Persist.Sql
import qualified Database.Persist.TH as PTH
import GHC.Generics (Generic)
import Text.Read

PTH.share
  [PTH.mkPersist PTH.sqlSettings {PTH.mpsGenerateLenses = True}, PTH.mkMigrate "migrateAll"]
  [PTH.persistLowerCase|
  OpeningPosition sql=opening_table
    pieceList PieceList
    nextToMove PieceColour
    whiteKingSideCastle Bool
    whiteQueenSideCastle Bool
    blackKingSideCastle Bool
    blackQueenSideCastle Bool
    enPassent EnPassentSquare
    evaluation Double
    Primary pieceList nextToMove whiteKingSideCastle whiteQueenSideCastle blackKingSideCastle blackQueenSideCastle enPassent
|]

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

fenToOpeningPositionKey :: FenRepresentation -> Key OpeningPosition
fenToOpeningPositionKey (FenRepresentation pl colour cst enP _ _) =
  OpeningPositionKey
    pl
    colour
    (cst ^. whiteKingSide)
    (cst ^. whiteQueenSide)
    (cst ^. blackKingSide)
    (cst ^. blackQueenSide)
    enP

getFenEvaluation fen = do
  openingPos <- PS.get $ fenToOpeningPositionKey fen
  return $ _openingPositionEvaluation <$> openingPos
