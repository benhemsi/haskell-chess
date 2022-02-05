{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Moves.MoveTree where
import Models.Position (Position)

data MoveTree a = EmptyTree | Leaf a | Node a [MoveTree a] deriving (Functor, Foldable, Traversable)

-- makeMove :: MoveTree Position -> MoveTree Position
-- makeMove 
