{-# LANGUAGE DeriveTraversable #-}

module Moves.MoveTree where
import Models.Position (Position)

data MoveTree a = EmptyTree | Leaf a | Node a [MoveTree a] deriving (Eq, Functor, Foldable, Traversable)

filterTree :: Eq a => (a -> Bool) -> MoveTree a -> MoveTree a
filterTree predicate tree = case tree of
  EmptyTree -> EmptyTree
  Leaf a -> if predicate a then Leaf a else EmptyTree
  Node a trees -> if predicate a then Node a (filter (EmptyTree /=) (map (filterTree predicate) trees)) else EmptyTree
