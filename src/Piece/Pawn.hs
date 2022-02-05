module Piece.Pawn where

import Models.File
import Models.Move
import Models.Rank
import Models.Square
import Piece.Moveable
import Data.Foldable (Foldable(toList))

data Pawn = P deriving (Show, Read)

instance Moveable Pawn where
  emptyBoardMoves P start = pawnMoves
    where
      startRank = rank start
      emptyPawnMoves = PM Nothing Nothing [] [] Nothing
      pawnMoves = case startRank of
        R1 -> PawnMoves emptyPawnMoves emptyPawnMoves
        R8 -> PawnMoves emptyPawnMoves emptyPawnMoves
        rank -> PawnMoves whiteMoves blackMoves
          where
            startFile = file start

            promotionWhite = case startRank of
              R7 -> Just $ PawnPromotion (Move start (Square startFile (succ startRank)))
              _ -> Nothing
            promotionBlack = case startRank of
              R2 -> Just $ PawnPromotion (Move start (Square startFile (pred startRank)))
              _ -> Nothing

            forwardWhite = case promotionWhite of
              Nothing -> Just $ Move start (Square startFile (succ startRank))
              Just _ -> Nothing
            forwardBlack = case promotionBlack of
              Nothing -> Just $ Move start (Square startFile (pred startRank))
              Just _ -> Nothing

            jumpWhite = case startRank of
              R2 -> Just $ Move start (Square startFile (succ $ succ startRank))
              _ -> Nothing
            jumpBlack = case startRank of
              R7 -> Just $ Move start (Square startFile (pred $ pred startRank))
              _ -> Nothing

            takeLeftWhite = case startFile of
              Fa -> Just $ Move start (Square (pred startFile) (succ startRank))
              _ -> Nothing
            takeLeftBlack = case startFile of
              Fa -> Just $ Move start (Square (pred startFile) (pred startRank))
              _ -> Nothing
            takeRightWhite = case startFile of
              Fh -> Just $ Move start (Square (succ startFile) (succ startRank))
              _ -> Nothing
            takeRightBlack = case startFile of
              Fh -> Just $ Move start (Square (succ startFile) (pred startRank))
              _ -> Nothing

            enPassentLeftWhite = case startRank of
              R5 -> fmap (\tl -> EnPassent tl (Square (pred . file . end $ tl) R6)) takeLeftWhite
              _ -> Nothing
            enPassentLeftBlack = case startRank of
              R4 -> fmap (\tl -> EnPassent tl (Square (pred . file . end $ tl) R3)) takeLeftBlack
              _ -> Nothing
            enPassentRightWhite = case startRank of
              R5 -> fmap (\tl -> EnPassent tl (Square (succ . file . end $ tl) R6)) takeRightWhite
              _ -> Nothing
            enPassentRightBlack = case startRank of
              R4 -> fmap (\tl -> EnPassent tl (Square (succ . file . end $ tl) R3)) takeRightBlack
              _ -> Nothing

            maybesToList mayA mayB = toList mayA ++ toList mayB

            takesWhite = maybesToList takeLeftWhite takeRightWhite
            takesBlack = maybesToList takeLeftBlack takeRightBlack

            enPassentsWhite = maybesToList enPassentLeftWhite enPassentRightWhite
            enPassentsBlack = maybesToList enPassentLeftBlack enPassentRightBlack

            whiteMoves = PM forwardWhite jumpWhite takesWhite enPassentsWhite promotionWhite
            blackMoves = PM forwardBlack jumpBlack takesBlack enPassentsBlack promotionBlack
