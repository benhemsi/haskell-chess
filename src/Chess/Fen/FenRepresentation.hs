{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Chess.Fen.FenRepresentation where

import Chess.Board
import Chess.Fen.CastlingPrivileges
import Chess.Fen.EnPassentSquare
import Chess.Piece
import Control.Lens
import Data.Aeson
import qualified Data.Map as Map
import GHC.Generics
import Test.QuickCheck

data FenRepresentation =
  FenRepresentation
    { _pieces :: PieceList
    , _nextToMove :: PieceColour
    , _castlingPrivileges :: CastlingPrivileges
    , _enPassentSquare :: EnPassentSquare
    , _halfMoveClock, _fullMoveClock :: Int
    }
  deriving (Eq, Generic, Ord)

makeLenses ''FenRepresentation

instance ToJSON FenRepresentation

instance FromJSON FenRepresentation

buildBaseFenRepresentation :: PieceList -> FenRepresentation
buildBaseFenRepresentation pl = FenRepresentation pl White (CastlingPrivileges True True True True) (EnPSq Nothing) 0 1

startingFenRepresentation :: FenRepresentation
startingFenRepresentation = buildBaseFenRepresentation startingPieceList

instance Show FenRepresentation where
  show (FenRepresentation pieces nextToMove castlingPrivileges enPassent halfMoveClock fullMoveClock) =
    show pieces ++
    " " ++
    show nextToMove ++
    " " ++ show castlingPrivileges ++ " " ++ show enPassent ++ " " ++ show halfMoveClock ++ " " ++ show fullMoveClock

likePieces :: Lens' FenRepresentation (Map.Map Square PieceType)
likePieces = likeLens (pieces . whitePieces) (pieces . blackPieces)

oppoPieces :: Lens' FenRepresentation (Map.Map Square PieceType)
oppoPieces = oppoLens (pieces . whitePieces) (pieces . blackPieces)

likeOccupiedSquares :: Getter FenRepresentation Squares
likeOccupiedSquares = likeGetter (pieces . whiteOccupiedSquares) (pieces . blackOccupiedSquares)

oppoOccupiedSquares :: Getter FenRepresentation Squares
oppoOccupiedSquares = oppoGetter (pieces . whiteOccupiedSquares) (pieces . blackOccupiedSquares)

likeKingSquare :: Lens' FenRepresentation Square
likeKingSquare = likeLens (pieces . whiteKingSquare) (pieces . blackKingSquare)

oppoKingSquare :: Lens' FenRepresentation Square
oppoKingSquare = oppoLens (pieces . whiteKingSquare) (pieces . blackKingSquare)

kingSidePrivileges :: Lens' FenRepresentation Bool
kingSidePrivileges = likeLens (castlingPrivileges . whiteKingSide) (castlingPrivileges . blackKingSide)

queenSidePrivileges :: Lens' FenRepresentation Bool
queenSidePrivileges = likeLens (castlingPrivileges . whiteQueenSide) (castlingPrivileges . blackQueenSide)

likeLens :: Lens' FenRepresentation a -> Lens' FenRepresentation a -> Lens' FenRepresentation a
likeLens whiteLens blackLens aToFa pos =
  let colour = view nextToMove pos
      output =
        case colour of
          White -> (\x -> set whiteLens x pos) <$> aToFa (pos ^. whiteLens)
          Black -> (\x -> set blackLens x pos) <$> aToFa (pos ^. blackLens)
   in output

oppoLens :: Lens' FenRepresentation a -> Lens' FenRepresentation a -> Lens' FenRepresentation a
oppoLens whiteLens blackLens = likeLens blackLens whiteLens

likeGetter :: Getter FenRepresentation a -> Getter FenRepresentation a -> Getter FenRepresentation a
likeGetter whiteGetter blackGetter aToFa pos =
  let colour = view nextToMove pos
      output =
        case colour of
          White -> pos <$ aToFa (pos ^. whiteGetter)
          Black -> pos <$ aToFa (pos ^. blackGetter)
   in output

oppoGetter :: Getter FenRepresentation a -> Getter FenRepresentation a -> Getter FenRepresentation a
oppoGetter whiteGetter blackGetter = likeGetter blackGetter whiteGetter

switchNextToMove :: FenRepresentation -> FenRepresentation
switchNextToMove pos = outputPos
  where
    currentColour = view nextToMove pos
    outputPos = set nextToMove (oppoColour currentColour) pos

instance Arbitrary FenRepresentation where
  arbitrary = do
    int <- chooseInt (2, 32)
    pl <- arbitrary
    nextToMove <- arbitrary
    castlingPrivileges <- arbitrary
    enPassent <- arbitrary
    halfMoveClock <- arbitrary
    fullMoveClock <- arbitrary
    return $ FenRepresentation pl nextToMove castlingPrivileges enPassent halfMoveClock fullMoveClock
