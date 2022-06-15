module Piece.Pawn where

import Control.Lens
import Data.Foldable (Foldable(toList))
import Models.File
import Models.Move
import Models.Rank
import Models.Square

data Pawn =
  P
  deriving (Show, Read)

instance Moveable Pawn where
  emptyBoardMoves P start = pawnMoves
    where
      startRank = _rank start
      emptyPawnMoves = PM Nothing Nothing [] [] Nothing []
      pawnMoves =
        case startRank of
          R1 -> PawnMoves emptyPawnMoves emptyPawnMoves
          R8 -> PawnMoves emptyPawnMoves emptyPawnMoves
          rank -> PawnMoves whiteMoves blackMoves
            where startFile = _file start
                  forwardWhite = Just $ Move start (Square startFile (succ startRank))
                  forwardBlack = Just $ Move start (Square startFile (pred startRank))
                  jumpWhite =
                    case startRank of
                      R2 -> Just $ Move start (Square startFile (succ $ succ startRank))
                      _ -> Nothing
                  jumpBlack =
                    case startRank of
                      R7 -> Just $ Move start (Square startFile (pred $ pred startRank))
                      _ -> Nothing
                  takeLeftWhite =
                    case startFile of
                      Fa -> Nothing
                      _ -> Just $ Move start (Square (pred startFile) (succ startRank))
                  takeLeftBlack =
                    case startFile of
                      Fa -> Nothing
                      _ -> Just $ Move start (Square (pred startFile) (pred startRank))
                  takeRightWhite =
                    case startFile of
                      Fh -> Nothing
                      _ -> Just $ Move start (Square (succ startFile) (succ startRank))
                  takeRightBlack =
                    case startFile of
                      Fh -> Nothing
                      _ -> Just $ Move start (Square (succ startFile) (pred startRank))
                  enPassentLeftWhite =
                    case startRank of
                      R5 -> fmap (\tl -> EnPassent tl (Square (view (end . file) tl) R5)) takeLeftWhite
                      _ -> Nothing
                  enPassentLeftBlack =
                    case startRank of
                      R4 -> fmap (\tl -> EnPassent tl (Square (view (end . file) tl) R4)) takeLeftBlack
                      _ -> Nothing
                  enPassentRightWhite =
                    case startRank of
                      R5 -> fmap (\tl -> EnPassent tl (Square (view (end . file) tl) R5)) takeRightWhite
                      _ -> Nothing
                  enPassentRightBlack =
                    case startRank of
                      R4 -> fmap (\tl -> EnPassent tl (Square (view (end . file) tl) R4)) takeRightBlack
                      _ -> Nothing
                  maybesToList mayA mayB = toList mayA ++ toList mayB
                  takesWhite = maybesToList takeLeftWhite takeRightWhite
                  takesBlack = maybesToList takeLeftBlack takeRightBlack
                  enPassentsWhite = maybesToList enPassentLeftWhite enPassentRightWhite
                  enPassentsBlack = maybesToList enPassentLeftBlack enPassentRightBlack
                  whiteMoves =
                    case startRank of
                      R7 ->
                        PM
                          Nothing
                          jumpWhite
                          []
                          enPassentsWhite
                          (fmap PawnPromotion forwardWhite)
                          (map PawnPromotion takesWhite)
                      _ -> PM forwardWhite jumpWhite takesWhite enPassentsWhite Nothing []
                  blackMoves =
                    case startRank of
                      R2 ->
                        PM
                          Nothing
                          jumpBlack
                          []
                          enPassentsBlack
                          (fmap PawnPromotion forwardBlack)
                          (map PawnPromotion takesBlack)
                      _ -> PM forwardBlack jumpBlack takesBlack enPassentsBlack Nothing []
