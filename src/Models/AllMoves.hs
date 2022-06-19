{-# LANGUAGE TemplateHaskell #-}

module Models.AllMoves where

import Control.Lens
import qualified Data.Map as Map
import Models.Move
import Models.Piece

data AllMoves =
  AllMoves
    { _likeMoves, _oppoMoves :: Map.Map PieceOnSquare [MoveTypes]
    }

makeLenses ''AllMoves
