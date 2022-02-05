module Evaluation.EvaluationBoard where

import Data.Array
import Data.List (groupBy)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Models.Board
import Models.FullPieceList
import Models.Move
import Models.PieceColour
import Models.PieceList
import Models.PieceOnSquare
import Models.Square
-- import Move.PieceMoves

data EvaluationSquare = EvaluationSquare {whiteAttackers, blackAttackers :: PieceList}

type EvaluationBoard = Board EvaluationSquare

-- getSquareAttackers :: PieceList -> FullPieceList -> Map.Map Square [PieceOnSquare]
-- getSquareAttackers pl fullPL = Map.fromList output
--   where
--     exploded = [(end move, piece) | piece <- pl, move <- validMoves piece fullPL]
--     grouped = groupBy (\a b -> fst a == fst b) exploded
--     output = [(fst h, map snd group) | group <- grouped, let h = head group, not (null group)]

-- buildEvaluationBoard :: FullPieceList -> EvaluationBoard
-- buildEvaluationBoard fullPL = array (minBound, maxBound) output
--   where
--     whiteSquareAttackers = getSquareAttackers (whitePieces fullPL) fullPL
--     blackSquareAttackers = getSquareAttackers (blackPieces fullPL) fullPL
--     output = do
--       i <- range (minBound, maxBound)
--       let whitePL = fromMaybe [] (Map.lookup i whiteSquareAttackers)
--           blackPL = fromMaybe [] (Map.lookup i blackSquareAttackers)
--       return (i, EvaluationSquare whitePL blackPL)

-- getAttackers :: Square -> PieceColour -> EvaluationBoard -> PieceList
-- getAttackers sq colour = getPieceList . (! sq)
--   where
--     getPieceList = case colour of
--       White -> whiteAttackers
--       Black -> blackAttackers

-- checkIfSquareAttacked :: Square -> PieceColour -> EvaluationBoard -> Bool
-- checkIfSquareAttacked sq colour = not . null . getAttackers sq colour

-- getNumberOfAttackers :: Square -> PieceColour -> EvaluationBoard -> Int
-- getNumberOfAttackers sq colour = length . getAttackers sq colour
