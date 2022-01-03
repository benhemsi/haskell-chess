module Models.PieceList where

import Models.Square (Square)
import Models.PieceOnSquare

type PieceList = [PieceOnSquare]

squareOccupied :: Square -> PieceList -> Bool
squareOccupied sq pl = sq `elem` map square pl
