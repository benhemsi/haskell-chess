module Chess.GameState where

import Chess.GameEnd
import Chess.PositionWithMoves

type GameState = Either GameEnd PositionWithMoves
