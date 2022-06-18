module Piece.King where

import Control.Lens
import Models.Move
import Models.Square
import Models.Weighted

data King =
  K
  deriving (Show, Read)

instance Moveable King where
  emptyBoardMoves K startSq = KingMoves output
    where
      startFile = _file startSq
      startRank = _rank startSq
      minFile = predFile startFile
      maxFile = succFile startFile
      minRank = predRank startRank
      maxRank = succRank startRank
      standardMoves =
        [ Move startSq endSq
        | f <- [minFile .. maxFile]
        , r <- [minRank .. maxRank]
        , let endSq = Square f r
        , endSq /= startSq
        ]
      kingSide = Castle (Move startSq (set file Fg startSq)) (Move (set file Fh startSq) (set file Ff startSq))
      queenSide = Castle (Move startSq (set file Fc startSq)) (Move (set file Fa startSq) (set file Fd startSq))
      output =
        case startSq of
          Square Fe R1 -> KM standardMoves (Just kingSide) (Just queenSide)
          Square Fe R8 -> KM standardMoves (Just kingSide) (Just queenSide)
          _ -> KM standardMoves Nothing Nothing

instance Weighted King where
  weight _ = 1000000
