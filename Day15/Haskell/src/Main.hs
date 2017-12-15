module Main where

import Data.Bits

data Generator = Generator { genMultiplier :: Int
                           , genCurrent :: Int
                           , genPredicate :: (Int -> Bool) }

mkGenerator :: Int -> Int -> (Int -> Bool) -> Generator
mkGenerator = Generator

genAdvance :: Generator -> Generator
genAdvance (Generator mp cur p) =
    if p (genCurrent nextGen)
        then nextGen
        else genAdvance nextGen
  where
    nextGen = Generator mp ((cur * mp) `rem` kFactor) p 
    kFactor = 0x7fffffff

puzzleGens1 :: (Generator, Generator)
puzzleGens1 = ( mkGenerator 16807 591 (const True)
              , mkGenerator 48271 393 (const True))

puzzleGens2 :: (Generator, Generator)
puzzleGens2 = ( mkGenerator 16807 591 (evenlyDividable 4)
              , mkGenerator 48271 393 (evenlyDividable 8))
  where
    evenlyDividable n1 n2 = n2 `rem` n1 == 0

findMatches :: Int -> (Generator, Generator) -> Int
findMatches 0 _            = 0
findMatches n (genA, genB) =
  let
    (genA', genB') = (genAdvance genA, genAdvance genB)
    (lowerA, lowerB) = (genCurrent genA' .&. 0xffff, genCurrent genB' .&. 0xffff)
  in
    if lowerA == lowerB
      then 1 + findMatches (n - 1) (genA', genB')
      else findMatches (n - 1) (genA', genB')

maxGenerations1 = 40000000
maxGenerations2 =  5000000

main :: IO ()
main = do
    putStrLn $ "Part 1: " ++ (show $ findMatches maxGenerations1 puzzleGens1)
    putStrLn $ "Part 2: " ++ (show $ findMatches maxGenerations2 puzzleGens2)
