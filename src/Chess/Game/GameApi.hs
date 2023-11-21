module Chess.Game.GameApi where

import Chess.Fen
import Chess.Game.GameEnd
import Chess.Move

class GameApi m where
  makeMove :: FenRepresentation -> MoveTypes -> m (Either GameEnd FenRepresentation)
