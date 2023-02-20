{-# LANGUAGE DeriveTraversable #-}

module Moves.MoveTree where

import Control.Lens
import Data.Foldable
import qualified Data.Map as Map

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

-- makeMove :: Position -> MoveTypes -> Position
-- makeMove pos mv = 
--   let (startSq, endSq) = case mv of 
--                            Mv (Move start end) -> (start, end)
--       pl = pos^.fen.pieces
changeKey :: Ord k => k -> k -> Map.Map k a -> Map.Map k a
changeKey startKey endKey startingMap =
  let (value, deletionMap) = Map.updateLookupWithKey (\_ _ -> Nothing) startKey startingMap
      outputMap =
        case value of
          Nothing -> deletionMap
          Just x -> Map.insert endKey x deletionMap
   in outputMap
-- updateFen :: MoveTypes -> FenRepresentation -> FenRepresentation
-- updateFen (Mv (Move startSq endSq)) startingFen = 
