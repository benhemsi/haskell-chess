{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Models.PieceOnSquare (PieceOnSquare, PieceList, piece, square, pieceOnSquare) where

import Data.List (intercalate, sort)
import Data.List.Split (chunksOf, splitOn)
import Models.Piece (Piece)
import Models.Square (Square)
import Text.Read

data PieceOnSquare = PieceOnSquare
  { piece :: Piece,
    square :: Square
  }
  deriving (Eq)

type PieceList = [PieceOnSquare]

pieceOnSquare :: Piece -> Square -> PieceOnSquare
pieceOnSquare = PieceOnSquare

instance Ord PieceOnSquare where
  a <= b = square a <= square b

-- Takes a PieceList and returns a string with '1' for each empty square and show Piece for each occupied square
initialPass :: [PieceOnSquare] -> String
initialPass pl = initialPassRec [minBound .. maxBound] (sort pl)
  where
    initialPassRec (sq : sqs) (p : ps) = if square p == sq then (show . piece) p ++ initialPassRec sqs ps else '1' : initialPassRec sqs (p : ps)
    initialPassRec (sq : sqs) [] = '1' : initialPassRec sqs []
    initialPassRec [] _ = ""

-- Takes a string produced by initialPass and returns a valid FEN piece placement
secondaryPass :: String -> String
secondaryPass s = intercalate "/" fenRanks
  where
    foldRank s = reverse (foldRankRec [] s)
      where
        foldRankRec s [] = s
        foldRankRec [] (y : ys) = foldRankRec [y] ys
        foldRankRec (x : xs) ('1' : ys) =
          case (readMaybe [x] :: Maybe Int) of
            Just n -> foldRankRec (show n ++ xs) ys
            Nothing -> foldRankRec ('1' : xs) ys
        foldRankRec s (y : ys) = foldRankRec (y : s) ys
    grouped = reverse (chunksOf 8 s)
    fenRanks = map foldRank grouped

-- Inverse of initialPass
initialPass' :: String -> [PieceOnSquare]
initialPass' s = getPieces
  where
    zipped :: [(Square, Char)]
    zipped = zip [minBound .. maxBound] s
    getPieces =
      foldl
        ( \curr (sq, c) -> case (readMaybe [c] :: Maybe Piece) of
            Just p -> PieceOnSquare p sq : curr
            Nothing -> curr
        )
        []
        zipped

-- Inverse of secondaryPass
secondaryPass' :: String -> String
secondaryPass' s = concat (reverse out)
  where
    splitString = splitOn "/" s
    explode curr (x : xs) =
      case (readMaybe [x] :: Maybe Int) of
        Just n -> explode (replicate n '1' ++ curr) xs
        Nothing -> explode (x : curr) xs
    explode curr [] = curr
    out = map (reverse . explode []) splitString

instance Show PieceOnSquare where
  show p = (show . piece) p ++ (show . square) p
  showList pl s = (secondaryPass . initialPass) pl ++ s

instance Read PieceOnSquare where
  readPrec =
    do
      Ident (f : r) <- lexP
      let piece = readMaybe [f]
          square = readMaybe r
      case (piece, square) of
        (Just p, Just sq) -> return (PieceOnSquare p sq)
        _ -> pfail

  readListPrec = do
    Ident s <- lexP
    return ((initialPass' . secondaryPass') s)
