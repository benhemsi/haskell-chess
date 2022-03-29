{-# LANGUAGE TemplateHaskell #-}

module Models.Piece where

import Control.Lens
import Data.Char (isUpper, toLower, toUpper)
import Models.Move
import Models.PieceColour
import Models.PieceType
import Text.Read

data Piece =
  Piece
    { _pieceColour :: PieceColour
    , _pieceType :: PieceType
    }
  deriving (Eq)

makeLenses ''Piece

instance Show Piece where
  show (Piece pieceColour pieceType) =
    if pieceColour == White
      then map toUpper (show pieceType)
      else map toLower (show pieceType)

instance Read Piece where
  readPrec = do
    Ident s <- lexP
    let colour =
          if all isUpper s
            then White
            else Black
        piece = readMaybe (map toUpper s)
    case piece of
      Just p -> return (Piece colour p)
      Nothing -> pfail
  readListPrec = readListPrecDefault
  readList = readListDefault

instance Moveable Piece where
  emptyBoardMoves (Piece _ tpe) = emptyBoardMoves tpe
