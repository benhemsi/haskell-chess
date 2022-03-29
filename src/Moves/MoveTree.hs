{-# LANGUAGE DeriveTraversable #-}

module Moves.MoveTree where

import Data.Foldable

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
    Node x trees ->
      Node x (filter (EmptyTree /=) (map (filterLeaves predicate) trees))

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
      (Node _ treesA, Node _ treesB) ->
        foldl' (<>) EmptyTree treesA <> foldl' (<>) EmptyTree treesB

instance Semigroup a => Monoid (MoveTree a) where
  mempty = EmptyTree
