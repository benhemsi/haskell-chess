module Models.Move where

import Models.Square

data Move = Move {start, end :: Square} deriving (Eq, Show)

type Moves = [Move]
