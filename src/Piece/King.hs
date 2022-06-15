module Piece.King where

import Control.Lens
import Models.File
import Models.Move
import Models.Rank
import Models.Square

data King =
  K
  deriving (Show, Read)

instance Moveable King where
  emptyBoardMoves K start = KingMoves output
    where
      startFile = _file start
      startRank = _rank start
      minFile = predFile startFile
      maxFile = succFile startFile
      minRank = predRank startRank
      maxRank = succRank startRank
      standardMoves =
        [ Move start endSq
        | f <- [minFile .. maxFile]
        , r <- [minRank .. maxRank]
        , let endSq = Square f r
        , endSq /= start
        ]
      kingSide = Castle (Move start (set file Fg start)) (Move (set file Fh start) (set file Ff start))
      queenSide = Castle (Move start (set file Fc start)) (Move (set file Fa start) (set file Fd start))
      output =
        case start of
          Square Fe R1 -> KM standardMoves (Just kingSide) (Just queenSide)
          Square Fe R8 -> KM standardMoves (Just kingSide) (Just queenSide)
          _ -> KM standardMoves Nothing Nothing
