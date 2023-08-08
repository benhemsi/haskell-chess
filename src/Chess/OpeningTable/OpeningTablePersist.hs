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

module Chess.OpeningTable.OpeningTablePersist where

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
  [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"]
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
    deriving Show
|]

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

fenWithEvalToOpeningPosition :: FenRepresentation -> Double -> (Key OpeningPosition, OpeningPosition)
fenWithEvalToOpeningPosition fen evaluation = (key, openingPos)
  where
    key = fenToOpeningPositionKey fen
    openingPos =
      OpeningPosition
        { openingPositionPieceList = fen ^. pieces
        , openingPositionNextToMove = fen ^. nextToMove
        , openingPositionWhiteKingSideCastle = fen ^. castlingPrivileges . whiteKingSide
        , openingPositionWhiteQueenSideCastle = fen ^. castlingPrivileges . whiteQueenSide
        , openingPositionBlackKingSideCastle = fen ^. castlingPrivileges . blackKingSide
        , openingPositionBlackQueenSideCastle = fen ^. castlingPrivileges . blackQueenSide
        , openingPositionEnPassent = fen ^. enPassentSquare
        , openingPositionEvaluation = evaluation
        }
