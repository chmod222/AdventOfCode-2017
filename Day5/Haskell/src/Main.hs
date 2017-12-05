module Main where

import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Vector.Unboxed as V

import Control.Monad.ST
import Control.Monad.Primitive

type IndexToucher = Int -> Int

escapeJumplist :: V.Vector Int -> IndexToucher -> Int
escapeJumplist list fn = runST $ do
    vec <- V.thaw list
    escape' vec 0

  where
    escape' vec idx = do
      newIdx <- jump vec idx fn

      if newIdx >= VM.length vec
        then
          return 1
        else do
          nn <- escape' vec newIdx
          return $ 1 + nn

jump :: (PrimMonad m) =>
     V.MVector (PrimState m) Int
  -> Int
  -> IndexToucher
  -> m Int
jump vec pos fn = do
  jumpLen <- VM.read vec pos
  VM.modify vec fn pos

  return $ pos + jumpLen

readJumplist :: String -> V.Vector Int
readJumplist = V.fromList . (map read) . lines

main :: IO ()
main = do
  input <- fmap readJumplist getContents

  putStrLn $ "Part 1: " ++ (show $ escapeJumplist input succ)
  putStrLn $ "Part 2: " ++ (show $ escapeJumplist input
    (\idx -> if idx > 2
             then pred idx
             else succ idx))
