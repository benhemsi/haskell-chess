module Models.Weighted where

class Weighted p where
  weight :: p -> Int
