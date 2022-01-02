module Models.PieceList where

import Data.Set (Set, fromList)
import Models.Piece hiding (piece)
import Models.PieceColour
import Models.PieceOnSquare (PieceOnSquare, piece, square)
import Models.Square (Square)
import Text.Read

type PieceList = [PieceOnSquare]

-- recA :: PieceList -> String
-- recA pl = 
--   let sortedList = sort pl
--       foo =       
--    in ""
-- instance Show PieceList where
--   show pl = let sortedList = sort pl
--              in 

-- type SquareSet = Set Square

-- data FullPieceList = FullPieceList
--   { whitePieces, blackPieces :: PieceList,
--     whiteOccupiedSquares, blackOccupiedSquares, whiteAttackedSquares, blackAttackedSquares :: SquareSet
--   }

-- convertPieceList :: PieceList -> FullPieceList
-- convertPieceList pieceList = FullPieceList whitePieces blackPieces whiteOccupiedSquares blackOccupiedSquares whiteAttackedSquares blackAttackedSquares
--   where
--     whitePieces = filter (\p -> (pieceColour . piece) p == White) pieceList
--     blackPieces = filter (\p -> (pieceColour . piece) p == Black) pieceList
--     whiteOccupiedSquares = fromList (map square whitePieces)
--     blackOccupiedSquares = fromList (map square whitePieces)
--     whiteAttackedSquares = fromList (map square whitePieces)
--     blackAttackedSquares = fromList (map square whitePieces)
