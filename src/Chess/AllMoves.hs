{-# LANGUAGE TemplateHaskell #-}

module Chess.AllMoves where

import Control.Lens
import qualified Data.Map as Map
import Chess.Move
import Chess.Piece

data AllMoves =
  AllMoves
    { _likeMoves, _oppoMoves :: Map.Map PieceOnSquare [MoveTypes]
    }

makeLenses ''AllMoves
