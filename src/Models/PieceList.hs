module Models.PieceList where

import Models.File
import Models.Piece
import Models.PieceOnSquare
import Models.Rank
import Models.Square

type PieceList = [PieceOnSquare]

startingPieceList :: PieceList
startingPieceList = pawns ++ knights ++ bishops ++ rooks ++ queens ++ kings
  where
    getPieces :: Piece -> Rank -> [File] -> [PieceOnSquare]
    getPieces pce rnk files = [PieceOnSquare pce (Square fle rnk) | fle <- files]
    whitePawns = getPieces whitePawn R2 [Fa .. Fh]
    blackPawns = getPieces blackPawn R7 [Fa .. Fh]
    pawns = whitePawns ++ blackPawns
    whiteKnights = getPieces whiteKnight R1 [Fb, Fg]
    blackKnights = getPieces blackKnight R8 [Fb, Fg]
    knights = whiteKnights ++ blackKnights
    whiteBishops = getPieces whiteBishop R1 [Fc, Ff]
    blackBishops = getPieces blackBishop R8 [Fc, Ff]
    bishops = whiteBishops ++ blackBishops
    whiteRooks = getPieces whiteRook R1 [Fa, Fh]
    blackRooks = getPieces blackRook R8 [Fa, Fh]
    rooks = whiteRooks ++ blackRooks
    whiteQueens = getPieces whiteQueen R1 [Fd]
    blackQueens = getPieces blackQueen R8 [Fd]
    queens = whiteQueens ++ blackQueens
    whiteKings = getPieces whiteKing R1 [Fe]
    blackKings = getPieces blackKing R8 [Fe]
    kings = whiteKings ++ blackKings
