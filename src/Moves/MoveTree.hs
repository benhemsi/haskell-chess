{-# LANGUAGE DeriveTraversable #-}

module Moves.MoveTree where

import Control.Lens
import Data.Foldable
import qualified Data.Map as Map
import Chess.Board
import Chess.Fen.FenRepresentation
import Chess.Move
import Chess.Piece
import Chess.Position

data MoveTree a
  = EmptyTree
  | Leaf a
  | Node a [MoveTree a]
  deriving (Eq, Functor, Foldable, Traversable)

filterLeaves :: Eq a => (a -> Bool) -> MoveTree a -> MoveTree a
filterLeaves predicate tree =
  case tree of
    EmptyTree -> EmptyTree
    Leaf x ->
      if predicate x
        then Leaf x
        else EmptyTree
    Node x [] -> filterLeaves predicate (Leaf x)
    Node x [EmptyTree] -> filterLeaves predicate (Leaf x)
    Node x trees -> Node x (filter (EmptyTree /=) (map (filterLeaves predicate) trees))

evaluateTree :: Monoid a => MoveTree a -> a
evaluateTree EmptyTree = mempty
evaluateTree (Leaf x) = x
evaluateTree (Node x []) = x
evaluateTree (Node _ xs) = foldl1 (<>) (map evaluateTree xs)

instance Semigroup a => Semigroup (MoveTree a) where
  tree1 <> tree2 =
    case (tree1, tree2) of
      (x, EmptyTree) -> x
      (EmptyTree, y) -> y
      (Leaf x, Leaf y) -> Leaf (x <> y)
      (Node x [], Node y []) -> Leaf (x <> y)
      (Node x [EmptyTree], Node y [EmptyTree]) -> Leaf (x <> y)
      (Node x [], Leaf y) -> Leaf (x <> y)
      (Leaf x, Node y []) -> Leaf (x <> y)
      (Node x [EmptyTree], Leaf y) -> Leaf (x <> y)
      (Leaf x, Node y [EmptyTree]) -> Leaf (x <> y)
      (Node _ [Leaf x], Leaf y) -> Leaf (x <> y)
      (Leaf x, Node _ [Leaf y]) -> Leaf (x <> y)
      (Node _ [Leaf x], Node _ [Leaf y]) -> Leaf (x <> y)
      (Leaf x, Node _ trees) -> Leaf x <> foldl' (<>) EmptyTree trees
      (Node _ trees, Leaf y) -> foldl' (<>) EmptyTree trees <> Leaf y
      (Node _ treesA, Node _ treesB) -> foldl' (<>) EmptyTree treesA <> foldl' (<>) EmptyTree treesB

instance Semigroup a => Monoid (MoveTree a) where
  mempty = EmptyTree

makeMove :: MoveTypes -> Position -> Position
makeMove mv pos =
  let (startSq, endSq) =
        case mv of
          Mv (Move start end) -> (start, end)
      startingColour = pos ^. fen . nextToMove
      fullMoveIncrement =
        case startingColour of
          Black -> (+ 1)
          White -> id
      fullMoveUpdate = over (fen . fullMoveClock) fullMoveIncrement
      startingPieceType = (pos ^. likePieces) Map.! startSq
      halfMoveIncrement =
        if startingPieceType == Pawn || Map.member endSq (pos ^. oppoPieces)
          then const 0
          else (+ 1)
      halfMoveUpdate = over (fen . halfMoveClock) halfMoveIncrement
      startRankEnum = fromEnum (startSq ^. rank)
      endRankEnum = fromEnum (endSq ^. rank)
      enPassentSq =
        if startingPieceType == Pawn && abs (startRankEnum - endRankEnum) == 2
          then let enPRank = toEnum ((startRankEnum + endRankEnum) `div` 2)
                in Just $ Square (startSq ^. file) enPRank
          else Nothing
      enPassentUpdate = set (fen . enPassentSquare) enPassentSq
      pceColourUpdate = over (fen . nextToMove) oppoColour
      likePiecesUpdate = over likePieces (changeKey startSq endSq)
      oppoPiecesUpdate = over oppoPieces (Map.delete endSq)
      kingSqUpdate =
        if startingPieceType == King
          then set likeKingSquare endSq
          else id
      fullUpdate =
        pceColourUpdate . likePiecesUpdate . oppoPiecesUpdate . fullMoveUpdate . halfMoveUpdate . enPassentUpdate
   in fullUpdate pos

changeKey :: Ord k => k -> k -> Map.Map k a -> Map.Map k a
changeKey startKey endKey = Map.mapKeys updateFunction
  where
    updateFunction =
      \k ->
        if k == startKey
          then endKey
          else k
