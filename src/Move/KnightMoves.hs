module Move.KnightMoves where

import Models.Move
import Models.Square

type KnightMoves = Moves

emptyBoardMoves :: Square -> KnightMoves
emptyBoardMoves start =
  let startFile = fromEnum (file start)
      startRank = fromEnum (rank start)
   in [ Move start (square (toEnum f) (toEnum r))
        | i <- [-2, -1, 1, 2],
          j <- [-2, -1, 1, 2],
          abs (i * j) == 2,
          let f = startFile + i
              r = startRank + j,
          f >= 0 && f <= 8,
          r >= 0 && r <= 8
      ]
