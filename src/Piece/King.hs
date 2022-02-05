module Piece.King where

import Models.File
import Models.Move
import Models.Rank
import Models.Square
import Piece.Moveable

data King = K deriving (Show, Read)

instance Moveable King where
  emptyBoardMoves K start =
    let startFile = _file start
        startRank = _rank start
        minFile = predFile startFile
        maxFile = succFile startFile
        minRank = predRank startRank
        maxRank = succRank startRank
     in Moves [ Move start endSq
          | f <- [minFile .. maxFile],
            r <- [minRank .. maxRank],
            let endSq = Square f r,
            endSq /= start
        ]
-- TODO add castling
