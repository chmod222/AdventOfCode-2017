module Main where

import Control.Monad
import Data.List

import qualified Data.Set as S

-- Part 1
isValidPassphrase1 :: String -> Bool
isValidPassphrase1 passphrase =
  let
    allWords = words passphrase
    wordSet = S.fromList $ allWords
  in
    S.size wordSet == length allWords

-- Part 2
isAnagram :: String -> String -> Bool
isAnagram a b = sort a == sort b

isValidPassphrase2 :: String -> Bool
isValidPassphrase2 passphrase =
  let
    allWords = words passphrase
  in
    isValidPassphrase1 passphrase
      && (not $ or [a /= b && isAnagram a b | a <- allWords, b <- allWords])

-- Common
main :: IO ()
main = do
  input <- fmap lines getContents

  let valid1 = filter isValidPassphrase1 input
  let valid2 = filter isValidPassphrase2 input

  putStrLn $ "Part 1: " ++ (show . length $ valid1)
  putStrLn $ "Part 2: " ++ (show . length $ valid2)
