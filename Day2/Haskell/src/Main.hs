module Main where

import Data.String (words)

-- Part 1
aocPart1 :: [[Int]] -> Int
aocPart1 = sum . map (uncurry getRowSum . getMinMax)

getMinMax :: (Bounded n, Ord n) => [n] -> (n, n)
getMinMax = foldr foldFunc (maxBound, minBound)
  where
    foldFunc n (min', max') = (min n min', max n max')

getRowSum :: (Ord n, Num n) => n -> n -> n
getRowSum min max = max - min

-- Part 2
aocPart2 :: [[Int]] -> Int
aocPart2 = sum . map (uncurry getRowDivision . getEvenlyDividable)

getEvenlyDividable :: (Integral n, Eq n) => [n] -> (n, n)
getEvenlyDividable = head . filter (uncurry isEvenlyDividable) . elementPairs
  where
    elementPairs :: [n] -> [(n, n)]
    elementPairs ns = [(x, y) | x <- ns, y <- ns]

    isEvenlyDividable :: (Integral n, Eq n) => n -> n -> Bool
    isEvenlyDividable a b = a /= b && b `rem` a == 0

getRowDivision :: (Ord n, Integral n) => n -> n -> n
getRowDivision a b = quot b a

-- Common
splitInput :: String -> [[Int]]
splitInput = map ((map read) . words) . lines

main :: IO ()
main = do
  input <- fmap splitInput getContents 

  putStrLn $ "Part 1: " ++ (show $ aocPart1 input)
  putStrLn $ "Part 2: " ++ (show $ aocPart2 input)
