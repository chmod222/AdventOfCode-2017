module Main where

import Control.Monad

type Vector a = (a, a, a)

splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy _ [] = []
splitBy delim lst = r : splitBy delim (drop 1 rs)
  where
    (r, rs) = break (== delim) lst

parseMovement :: (Num a) => String -> Vector a
--                     X   Y   Z
parseMovement "n"  = ( 0,  1, -1)
parseMovement "ne" = ( 1,  0, -1)
parseMovement "se" = ( 1, -1,  0)
parseMovement "s"  = ( 0, -1,  1)
parseMovement "sw" = (-1,  0,  1)
parseMovement "nw" = (-1,  1,  0)
parseMovement _    = undefined

cubeDistance :: (Num a, Fractional a) => Vector a -> Vector a -> a
cubeDistance (x1, y1, z1) (x2, y2, z2) = (abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)) / 2

addCoord :: (Num a) => Vector a -> Vector a -> Vector a
addCoord (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

walkPath :: (Num a, Fractional a) => [Vector a] -> [(Vector a, a)]
walkPath = scanl walkFunc (origin, 0)
  where
    walkFunc (pos, dist) pos' = (newPos, cubeDistance origin newPos)
      where
        newPos = addCoord pos pos'

    origin = (0, 0, 0)

main :: IO ()
main = do
    input <- fmap (map (map parseMovement . splitBy ',') . lines) getContents

    forM_ input (\track -> do
        let waypoints = walkPath track

        putStrLn $ "Part 1: Stop distance: " ++ (show $ snd . last $ waypoints)
        putStrLn $ "Part 2: Max distance: " ++ (show $ maximum . map snd $ waypoints))
