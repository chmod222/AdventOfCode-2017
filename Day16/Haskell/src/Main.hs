{-# LANGUAGE BangPatterns #-}
module Main where

import Data.List

data Pattern = Spin Int
             | Swap Int Int
             | SwapBy Char Char
    deriving (Show)

splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy _ [] = []
splitBy delim lst = r : splitBy delim (drop 1 rs)
  where
    (r, rs) = break (== delim) lst

parsePattern :: String -> Pattern
parsePattern ('s':n) = Spin (read n)
parsePattern ('x':r) = Swap (read x) (read y)
  where
    (x:y:_) = splitBy '/' r

parsePattern ('p':r) = SwapBy x y
  where
    ((x:_):(y:_): _) = splitBy '/' r

danceGroup :: String
danceGroup = ['a' .. 'p']

dance :: String -> Pattern -> String
dance group (Spin n) = rotate (length group - n) group
  where
    rotate !n !g = take (length g) (drop n (cycle g))

dance group (Swap x y) = swapTwo' x y group
dance group (SwapBy xn yn) = dance group (Swap x y)
  where
    (!x:_) = findIndices (== xn) group
    (!y:_) = findIndices (== yn) group

swapTwo' f s xs = zipWith (\x y -> 
    if x == f then xs !! s
    else if x == s then xs !! f
    else y) [0..] xs


performDance :: String -> [Pattern] -> String
performDance grp = foldl' (\grp pat -> dance grp pat) grp

findCycle :: String -> [Pattern] -> Int
findCycle grp pat = (1 +) . length . takeWhile (/= grp) . drop 1 $ iterate (\g -> performDance g pat) grp

main :: IO ()
main = do
    input <- fmap (head . map (map parsePattern) . map (splitBy ',') . lines) getContents

    let finalState1 = performDance danceGroup input
    putStrLn $ "Part 1: " ++ finalState1

    let cycleLength = findCycle danceGroup input
    let iterations = 1000000000 - (1000000000 `quot` cycleLength) * cycleLength

    putStrLn $ "Part 2: " ++ (last . take (succ iterations) $ iterate (\g -> performDance g input) danceGroup)
