{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Models.Piece.PieceList where

import Control.Lens
import Data.List (intercalate, sort, sortBy)
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace (trace)
import Models.Board
import Models.Piece.Piece
import Test.QuickCheck
import Text.Read

type PieceList = Map.Map Square Piece

startingPieceList :: PieceList
startingPieceList = Map.unions [pawns, knights, bishops, rooks, queens, kings]
  where
    getPieces :: Piece -> Rank -> [File] -> PieceList
    getPieces pce rnk files = Map.fromList [(Square fle rnk, pce) | fle <- files]
    whitePawns = getPieces whitePawn R2 [Fa .. Fh]
    blackPawns = getPieces blackPawn R7 [Fa .. Fh]
    pawns = whitePawns `Map.union` blackPawns
    whiteKnights = getPieces whiteKnight R1 [Fb, Fg]
    blackKnights = getPieces blackKnight R8 [Fb, Fg]
    knights = whiteKnights `Map.union` blackKnights
    whiteBishops = getPieces whiteBishop R1 [Fc, Ff]
    blackBishops = getPieces blackBishop R8 [Fc, Ff]
    bishops = whiteBishops `Map.union` blackBishops
    whiteRooks = getPieces whiteRook R1 [Fa, Fh]
    blackRooks = getPieces blackRook R8 [Fa, Fh]
    rooks = whiteRooks `Map.union` blackRooks
    whiteQueens = getPieces whiteQueen R1 [Fd]
    blackQueens = getPieces blackQueen R8 [Fd]
    queens = whiteQueens `Map.union` blackQueens
    whiteKings = getPieces whiteKing R1 [Fe]
    blackKings = getPieces blackKing R8 [Fe]
    kings = whiteKings `Map.union` blackKings

-- Takes a PieceList and returns a string with '1' for each empty square and show Piece for each occupied square
initialPass :: PieceList -> String
initialPass pl = initialPassRec [minBound .. maxBound] (Map.assocs pl)
  where
    initialPassRec (sq:sqs) (p:ps) =
      if fst p == sq
        then show (snd p) ++ initialPassRec sqs ps
        else '1' : initialPassRec sqs (p : ps)
    initialPassRec (_:sqs) [] = '1' : initialPassRec sqs []
    initialPassRec [] _ = ""

-- Takes a string produced by initialPass and returns a valid FEN piece placement
secondaryPass :: String -> String
secondaryPass s = intercalate "/" fenRanks
  where
    foldRank s = reverse (foldRankRec [] s)
      where
        foldRankRec s [] = s
        foldRankRec [] (y:ys) = foldRankRec [y] ys
        foldRankRec (x:xs) ('1':ys) =
          case (readMaybe [x] :: Maybe Int) of
            Just n -> foldRankRec (show (n + 1) ++ xs) ys
            Nothing -> foldRankRec ('1' : x : xs) ys
        foldRankRec s (y:ys) = foldRankRec (y : s) ys
    grouped = reverse (chunksOf 8 s)
    fenRanks = map foldRank grouped

-- Inverse of initialPass
initialPass' :: String -> PieceList
initialPass' s = getPieces
  where
    zipped :: Map.Map Square Char
    zipped = Map.fromList $ zip [minBound .. maxBound] s
    getPieces = Map.mapMaybe (\c -> readMaybe [c]) zipped

-- Inverse of secondaryPass
secondaryPass' :: String -> String
secondaryPass' s = concat (reverse out)
  where
    splitString = splitOn "/" s
    explode curr (x:xs) =
      case (readMaybe [x] :: Maybe Int) of
        Just n -> explode (replicate n '1' ++ curr) xs
        Nothing -> explode (x : curr) xs
    explode curr [] = curr
    out = map (reverse . explode []) splitString

occupiedSquares :: Getter PieceList Squares
occupiedSquares a2fa pl = pl <$ a2fa (Map.keysSet pl)

instance Show PieceList where
  show = secondaryPass . initialPass
-- instance Read PieceList where
--   readPrec = do
--     Ident s <- lexP
--     return $ trace ("hello" ++ s) (initialPass' $ secondaryPass' s)
