{-# LANGUAGE TemplateHaskell #-}

module Chess.PositionWithMoves where

import Control.Lens
import Chess.AllMoves
import Chess.Position

data PositionWithMoves =
  PositionWithMoves
    { _position :: Position
    , _allMoves :: AllMoves
    }

makeLenses ''PositionWithMoves
