module Models.Piece (Piece (..)) where

import Data.Char (isUpper, toLower, toUpper)
import Models.PieceColour
import Models.PieceType

data Piece p = Piece {pieceColour :: PieceColour, pieceType :: PieceType p}

-- instance Show Piece where
--   show (Piece pieceColour pieceType) =
--     if pieceColour == White
--       then map toUpper (show pieceType)
--       else map toLower (show pieceType)

-- instance Read Piece where
--   readPrec =
--     do
--       Ident s <- lexP
--       let colour = if all isUpper s then White else Black
--           piece = readMaybe (map toUpper s)
--       case piece of
--         Just p -> return (Piece colour p)
--         Nothing -> pfail

--   readListPrec = readListPrecDefault
--   readList = readListDefault
