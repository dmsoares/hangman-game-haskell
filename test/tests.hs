module Main where

import qualified Hangman as H
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "fillInCharacter" $ do
    it "adds character to filledIn and to guessed chars if it exists in word" $ do
      H.fillInCharacter (H.Puzzle "batata" (replicate 6 Nothing) "" 7) 't'
        `shouldBe` H.Puzzle "batata" [Nothing, Nothing, Just 't', Nothing, Just 't', Nothing] "t" 7
    it "adds char to guessed char" $ do
      property prop_fillInCharacter

puzzleGen :: Gen H.Puzzle
puzzleGen = do
  word <- arbitrary
  filledIn <- arbitrary
  guessed <- arbitrary
  lives <- arbitrary `suchThat` (> 0)
  return (H.Puzzle word filledIn guessed lives)

instance Arbitrary H.Puzzle
  where arbitrary = puzzleGen

prop_fillInCharacter :: H.Puzzle -> Char -> Bool
prop_fillInCharacter puzzle@(H.Puzzle _ _ g _) c =
  let puzzle' = H.fillInCharacter puzzle c
  in
    length g < (\(H.Puzzle _ _ g' _) -> length g') puzzle'
