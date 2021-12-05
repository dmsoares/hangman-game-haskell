module Main where

import Hangman

main :: IO ()
main = do
  putStrLn "something"

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word
                 filledInSoFar s 7) c =
  Puzzle word newFilledInSoFar (c : s) 7
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar =
          let zd = (zipper c)
          in zipWith zd word filledInSoFar
