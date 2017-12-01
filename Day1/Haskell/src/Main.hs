module Main where

import Data.Char (digitToInt)

-- Part 1
sumDuplicates' :: [Int] -> Int
sumDuplicates' (x:[]) = 0
sumDuplicates' (x:y:rs)
  | x == y    = x + sumDuplicates' (y:rs)
  | otherwise = sumDuplicates' (y:rs)

sumDuplicates :: [Int] -> Int
sumDuplicates xs = sumDuplicates' $ xs ++ take 1 xs

-- Part 2
sumDuplicatesHalfway :: [Int] -> Int
sumDuplicatesHalfway xs = sumDuplicatesHalfway' 0 (length xs) (cycle xs)

sumDuplicatesHalfway' :: Int -> Int -> [Int] -> Int
sumDuplicatesHalfway' i n xs
  | i == n          = 0
  | current == next = current + sumDuplicatesHalfway' (succ i) n xs
  | otherwise       = sumDuplicatesHalfway' (succ i) n xs

  where
    current = xs !! i
    next    = xs !! (i + quot n 2)

-- Common
splitInput :: String -> [Int]
splitInput = map digitToInt

main :: IO ()
main = do
  input <- fmap splitInput getLine 

  putStrLn $ "Part 1: " ++ (show $ sumDuplicates input)
  putStrLn $ "Part 2: " ++ (show $ sumDuplicatesHalfway input)
