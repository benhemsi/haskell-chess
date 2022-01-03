module Move.PawnMoves where

import qualified Data.Set as Set
import Models.File
import Models.Move
import Models.Rank
import Models.Square

data PawnMoves = PawnMoves {white :: Moves, black :: Moves} deriving (Show)

emptyBoardMoves :: Square -> PawnMoves
emptyBoardMoves start =
  let startRank = rank start
   in ( if startRank == R1 || startRank == R8
          then PawnMoves [] []
          else
            let startFile = file start
                forwardWhite = Move start (square startFile (succ startRank))
                forwardBlack = Move start (square startFile (pred startRank))
                takeLeftWhite = [Move start (square (pred startFile) (succ startRank)) | startFile /= Fa]
                takeLeftBlack = [Move start (square (pred startFile) (pred startRank)) | startFile /= Fa]
                takeRightWhite = [Move start (square (succ startFile) (succ startRank)) | startFile /= Fh]
                takeRightBlack = [Move start (square (succ startFile) (pred startRank)) | startFile /= Fh]
             in PawnMoves (forwardWhite : takeLeftWhite ++ takeRightWhite) (forwardBlack : takeLeftBlack ++ takeRightBlack)
      )

validMoves :: PawnMoves -> Squares -> Squares -> PawnMoves
validMoves pawnMoves whiteOccupiedSquares blackOccupiedSquares = PawnMoves (filterMoves (white pawnMoves) blackOccupiedSquares) (filterMoves (black pawnMoves) whiteOccupiedSquares)
  where
    filterMoves :: Moves -> Squares -> Moves
    filterMoves moves oppoOccupiedSquares =
      case moves of
        (forward : takeMoves) -> forward : filter (\move -> end move `Set.member` oppoOccupiedSquares) takeMoves
        [] -> []
