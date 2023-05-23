{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Chess.Board.Board where

import Data.Array
import Data.Distributive
import Data.Functor.Rep
import Chess.Board.Square

type Board = Array Square

instance Distributive Board where
  collect f fa = arr
    where
      repF :: (a -> Board b) -> Square -> a -> b
      repF f = flip ((!) . f)
      arr = array (minBound, maxBound) [(i, fb) | i <- range (minBound, maxBound), let fb = fmap (repF f i) fa]

instance Representable Board where
  type Rep Board = Square
  tabulate f = array (minBound, maxBound) [(i, f i) | i <- range (minBound, maxBound)]
  index = (!)
