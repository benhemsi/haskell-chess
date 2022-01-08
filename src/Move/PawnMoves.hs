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
                forwardWhite = Move start (Square startFile (succ startRank))
                forwardBlack = Move start (Square startFile (pred startRank))
                takeLeftWhite = [Move start (Square (pred startFile) (succ startRank)) | startFile /= Fa]
                takeLeftBlack = [Move start (Square (pred startFile) (pred startRank)) | startFile /= Fa]
                takeRightWhite = [Move start (Square (succ startFile) (succ startRank)) | startFile /= Fh]
                takeRightBlack = [Move start (Square (succ startFile) (pred startRank)) | startFile /= Fh]
             in PawnMoves (forwardWhite : takeLeftWhite ++ takeRightWhite) (forwardBlack : takeLeftBlack ++ takeRightBlack)
      )

validMoves :: PawnMoves -> Squares -> Squares -> PawnMoves
validMoves pawnMoves whiteOccupiedSquares blackOccupiedSquares = PawnMoves (filterMoves (white pawnMoves) whiteOccupiedSquares blackOccupiedSquares) (filterMoves (black pawnMoves) blackOccupiedSquares whiteOccupiedSquares)
  where
    filterMoves :: Moves -> Squares -> Squares -> Moves
    filterMoves moves likeOccupiedSquares oppoOccupiedSquares =
      case moves of
        (forward : takeMoves) -> [forward | end forward `Set.notMember` (likeOccupiedSquares `Set.union` oppoOccupiedSquares)] ++ filter (\move -> end move `Set.member` oppoOccupiedSquares) takeMoves
        [] -> []
