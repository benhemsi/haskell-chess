{-# LANGUAGE DeriveTraversable #-}

module Moves.MoveTree where

import Data.Foldable

data MoveTree a = EmptyTree | Leaf a | Node a [MoveTree a] deriving (Eq, Functor, Foldable, Traversable)

filterTree :: Eq a => (a -> Bool) -> MoveTree a -> MoveTree a
filterTree predicate tree = case tree of
  EmptyTree -> EmptyTree
  Leaf a -> if predicate a then Leaf a else EmptyTree
  Node a trees -> if predicate a then Node a (filter (EmptyTree /=) (map (filterTree predicate) trees)) else EmptyTree

instance Semigroup a => Semigroup (MoveTree a) where
  tree1 <> tree2 = case (tree1, tree2) of
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
