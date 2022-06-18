module Models.GameState where

import Models.GameEnd
import Models.PositionWithMoves

type GameState = Either GameEnd PositionWithMoves
