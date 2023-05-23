module Chess.Weighted where

class Weighted p where
  weight :: p -> Int
