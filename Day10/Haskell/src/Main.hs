module Main where

import Numeric (showHex)

import Data.List
import Data.Char
import Data.Bits

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

main :: IO ()
main = do
    input <- getLine
    
    let initHash = [0..255]

    let part1Lens = map read . splitBy ',' $ input
    let (_, _, part1Res) = hashRound' 0 0 part1Lens initHash

    putStrLn $ "Part 1 Cecksum: " ++ (show $ (head part1Res) * (head . drop 1 $ part1Res))
    putStrLn $ "Part 2 Hash: " ++ (toHexString . knotHash (deriveLens input) $ initHash)
