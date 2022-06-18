{-# LANGUAGE TemplateHaskell #-}

module Models.PositionWithMoves where

import Control.Lens
import Models.AllMoves
import Models.Position

data PositionWithMoves =
  PositionWithMoves
    { _position :: Position
    , _allMoves :: AllMoves
    }

makeLenses ''PositionWithMoves
