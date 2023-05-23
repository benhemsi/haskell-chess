{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverlappingInstances #-}

module Chess.Piece.PieceList where

import Control.Lens
import Data.Aeson
import Data.List (intercalate, sort, sortBy)
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace (trace)
import GHC.Generics hiding (R1)
import Chess.Board
import Chess.Piece.Piece
import Chess.Piece.PieceColour
import Chess.Piece.PieceType
import Test.QuickCheck
import Text.Read

data PieceList =
  PieceList
    { _whitePieces, _blackPieces :: Map.Map Square PieceType
    , _whiteKingSquare, _blackKingSquare :: Square
    }
  deriving (Eq, Generic)

makeLenses ''PieceList

instance ToJSON PieceList

instance FromJSON PieceList

buildPieceList :: Map.Map Square Piece -> PieceList
buildPieceList pl = PieceList whitePcs blackPcs (whiteKing ^. _1) (blackKing ^. _1)
  where
    filterByColour colour = fmap _pieceType . Map.filter ((colour ==) . _pieceColour)
    whitePcs = filterByColour White pl
    blackPcs = filterByColour Black pl
    whiteKing = head $ filter (\p -> p ^. _2 == King) (Map.assocs whitePcs)
    blackKing = head $ filter (\p -> p ^. _2 == King) (Map.assocs blackPcs)

startingPieceList :: PieceList
startingPieceList = PieceList whitePcs blackPcs whiteKing blackKing
  where
    getPieces :: PieceType -> Rank -> [File] -> Map.Map Square PieceType
    getPieces pce rnk files = Map.fromList [(Square fle rnk, pce) | fle <- files]
    whitePawns = getPieces Pawn R2 [Fa .. Fh]
    blackPawns = getPieces Pawn R7 [Fa .. Fh]
    whiteKnights = getPieces Knight R1 [Fb, Fg]
    blackKnights = getPieces Knight R8 [Fb, Fg]
    whiteBishops = getPieces Bishop R1 [Fc, Ff]
    blackBishops = getPieces Bishop R8 [Fc, Ff]
    whiteRooks = getPieces Rook R1 [Fa, Fh]
    blackRooks = getPieces Rook R8 [Fa, Fh]
    whiteQueens = getPieces Queen R1 [Fd]
    blackQueens = getPieces Queen R8 [Fd]
    whiteKing = Square Fe R1
    blackKing = Square Fe R8
    whitePcs = Map.unions [whitePawns, whiteKnights, whiteBishops, whiteRooks, whiteQueens]
    blackPcs = Map.unions [blackPawns, blackKnights, blackBishops, blackRooks, blackQueens]

whiteOccupiedSquares :: Getter PieceList Squares
whiteOccupiedSquares a2fa pl = pl <$ a2fa (Set.insert (pl ^. whiteKingSquare) $ Map.keysSet (pl ^. whitePieces))

blackOccupiedSquares :: Getter PieceList Squares
blackOccupiedSquares a2fa pl = pl <$ a2fa (Set.insert (pl ^. blackKingSquare) $ Map.keysSet (pl ^. blackPieces))

-- Takes a PieceList and returns a string with '1' for each empty square and show Piece for each occupied square
initialPass :: PieceList -> String
initialPass pl = initialPassRec [minBound .. maxBound] (Map.assocs mapOfPieces)
  where
    kings = Map.fromList [(pl ^. whiteKingSquare, Piece White King), (pl ^. blackKingSquare, Piece Black King)]
    mapOfPieces =
      fmap (Piece White) (pl ^. whitePieces) `Map.union` fmap (Piece Black) (pl ^. blackPieces) `Map.union` kings
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
initialPass' s = PieceList whitePcs blackPcs whiteKing blackKing
  where
    zipped :: Map.Map Square Char
    zipped = Map.fromList $ zip [minBound .. maxBound] s
    allPieces = Map.mapMaybe (\c -> readMaybe [c]) zipped
    getPieceType = view pieceType
    whitePcs = fmap getPieceType $ Map.filter (\p -> p ^. pieceColour == White && p ^. pieceType /= King) allPieces
    blackPcs = fmap getPieceType $ Map.filter (\p -> p ^. pieceColour == Black && p ^. pieceType /= King) allPieces
    whiteKing = head $ Map.keys $ Map.filter (Piece White King ==) allPieces
    blackKing = head $ Map.keys $ Map.filter (Piece Black King ==) allPieces

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

instance Show PieceList where
  show = secondaryPass . initialPass

instance Arbitrary PieceList where
  arbitrary = do
    whiteKing <- arbitrary
    blackKing <- arbitrary
    whitePcs <- arbitrary
    blackPcs <- arbitrary
    return $ PieceList whitePcs blackPcs whiteKing blackKing
