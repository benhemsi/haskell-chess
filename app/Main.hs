module Main where

import Data.Aeson
import Models.Fen.FenParser
import System.Exit

play :: IO ()
play = do
  let playAgain = do
        putStrLn "Would you like to play again? (y/n)"
        ch <- getLine
        if ch == "y"
          then play
          else exitSuccess
  putStrLn "Enter FEN:"
  str <- getLine
  case parseFen str of
    Right valid -> do
      print (encode valid)
      playAgain
    Left error -> do
      print error
      playAgain

main = play
