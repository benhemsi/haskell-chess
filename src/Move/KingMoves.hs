module Move.KingMoves where

import Models.File
import Models.Move
import Models.Rank
import Models.Square

type KingMoves = Moves

emptyBoardMoves :: Square -> KingMoves
emptyBoardMoves start =
  let startFile = file start
      startRank = rank start
      minFile = predFile startFile
      maxFile = succFile startFile
      minRank = predRank startRank
      maxRank = succRank startRank
   in [ Move start endSq
        | f <- [minFile .. maxFile],
          r <- [minRank .. maxRank],
          let endSq = square f r,
          endSq /= start
      ]
