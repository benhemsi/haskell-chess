{-# LANGUAGE RankNTypes #-}

module Models.PieceList where

import Models.PieceOnSquare

type PieceList = forall p. [PieceOnSquare p]
