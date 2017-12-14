module Main where

import Numeric

import Debug.Trace
import Data.List
import Data.Char
import Data.Maybe
import Data.Bits

{-
 - TODO: rewrite to bytestring
 -}

splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy _ [] = []
splitBy delim lst = r : splitBy delim (drop 1 rs)
  where
    (r, rs) = break (== delim) lst

-- Bit convoluted, but it works
mangle :: Int -> Int -> [Int] -> [Int]
mangle pos len input = newList
  where
    pos' = pos `rem` (length input)
    normalizedList = take (length input) . drop pos' . cycle $ input
    partiallyReversed = (reverse . take len $ normalizedList) ++ drop len normalizedList
    newList = take (length input) . drop (length input - pos') . cycle $ partiallyReversed

hashRound' pos skipSize []     input = (pos, skipSize, input)
hashRound' pos skipSize (l:ls) input =
    hashRound' (pos + skipSize + l) (succ skipSize) ls (mangle pos l input)

knotHash :: [Int] -> [Int] -> [Int]
knotHash lens input = compactHash $ knotHash' 0 0 lens input 64
  where
    compactHash = map (xorChunk) . chunkify 16
      where
        xorChunk = foldr1 xor
        chunkify _ [] = []
        chunkify n l
            | n > 0     = take n l : chunkify n (drop n l)
            | otherwise = undefined

    knotHash' pos skipSize lens input 0       = input
    knotHash' pos skipSize lens input roundNo = knotHash' rndPos rndSkip lens rndHash (pred roundNo)
      where 
        (rndPos, rndSkip, rndHash) = hashRound' pos skipSize lens input


toHexString :: [Int] -> String
toHexString = concat . map (addLeadingZero . flip showHex "")
  where
    addLeadingZero (x:[]) = '0' : x : []
    addLeadingZero  x     = x

deriveLens :: String -> [Int]
deriveLens = (++ [17, 31, 73, 47, 23]) . map ord

puzzleInput = "ffayrhll"

disk :: String -> [[Int]]
disk input = map (flip knotHash [0..255] . deriveLens . ((input ++ "-") ++) . show)  [0..127]

bitDisk :: [[Int]] -> [[Bool]]
bitDisk = map (concat . map (\n -> map (\p -> testBit n p) (reverse [0..7])))

neighbors :: Int -> Int -> [(Int, Int)]
neighbors x y = map (\(x', y') -> (x' + x, y' + y)) [(0, 1), (0, -1), (1, 0), (-1, 0)]

markVisited :: [[Bool]] -> Int -> Int -> [[Bool]]
markVisited m x y
    | m !! y !!  x = m
    | otherwise    = set m y (set (m !! y) x True)
  where
    -- really nasty
    set xs n x = take n xs ++ [x] ++ drop (n + 1)  xs

-- Lists is a *remarkably* bad data structure for this kind of task, but what the hell
countIslands :: [[Bool]] -> Int
countIslands input = snd $ foldl (\(s, n) y -> visitCols s 0 y n) (initVisited, 0) [0.. (length input - 1)]
  where
    visitCols state x y n
        | x == (length input) = (state, n)
        | otherwise           =
            if (not $ state !! y !! x) && (input !! y !! x)
                then visitCols (nextState state x y) (succ x) y (succ n)
                else visitCols state (succ x) y n

    nextState s1 x y = foldl (\s (x', y') ->
        if (not $ s !! y' !! x') && (input !! y' !! x')
            then nextState s x' y'
            else s)
        
        (markVisited s1 x y)
        (filter inRange $ neighbors x y)

    inRange (x', y') = x' >= 0 && x' < length input && y' >= 0 && y' < length input

    initVisited = replicate (length input) (replicate (length input) False)

main :: IO ()
main = do
    let disk' = disk puzzleInput

    putStrLn $ "Part 1: " ++ show (sum . map sum . map (map popCount) $ disk')
    putStrLn $ "Part 2: " ++ show (countIslands $ bitDisk disk')
