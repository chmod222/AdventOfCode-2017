module Main where

import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Vector.Unboxed as V

import Control.Monad.ST
import Control.Monad.Primitive

type IndexToucher = Int -> Int

-- Mutable Vectors
escapeJumplist' :: V.Vector Int -> IndexToucher -> Int
escapeJumplist' list fn = runST $ do
    vec <- V.thaw list
    escape' vec 0

  where
    escape' :: (PrimMonad m) => V.MVector (PrimState m) Int -> Int -> m Int
    escape' vec idx = do
      newIdx <- jump' vec idx fn

      if newIdx >= VM.length vec
        then return 1
        else succ `fmap` escape' vec newIdx

jump' :: (PrimMonad m) =>
     V.MVector (PrimState m) Int
  -> Int
  -> IndexToucher
  -> m Int
jump' vec pos fn = do
  jumpLen <- VM.unsafeRead vec pos
  VM.unsafeModify vec fn pos

  return $ pos + jumpLen

-- Traditional, about half as fast as ST mutable
escapeJumplist :: V.Vector Int -> IndexToucher -> Int
escapeJumplist vec fn = escape' vec 0
  where
    escape' vec idx =
      let
        (vec', idx') = jump vec idx fn
      in
        if idx' >= V.length vec
          then 1
          else succ $! escape' vec' idx'

jump :: V.Vector Int -> Int -> IndexToucher -> (V.Vector Int, Int)
jump vec pos fn = do
  let jumpLen = (V.!) vec pos
  
  (V.update vec (V.fromList [(pos, fn jumpLen)]), pos + jumpLen)

-- Common
readJumplist :: String -> V.Vector Int
readJumplist = V.fromList . (map read) . lines

part1Toucher :: IndexToucher
part1Toucher = succ

part2Toucher :: IndexToucher
part2Toucher idx
  | idx > 2   = pred idx
  | otherwise = succ idx

main :: IO ()
main = do
  input <- fmap readJumplist getContents

  putStrLn $ "Part 1: " ++ (show $ escapeJumplist' input part1Toucher)
  putStrLn $ "Part 2: " ++ (show $ escapeJumplist' input part2Toucher)
