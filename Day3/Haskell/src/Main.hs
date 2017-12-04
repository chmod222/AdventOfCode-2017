module Main (main) where

import Data.List
import Data.Maybe

type Coord = (Int, Int)
type MovementVector = Coord

puzzleInput :: Int
puzzleInput = 361527

moveRight = (0, 1)
moveLeft = (0, -1)
moveUp = (-1, 0)
moveDown = (1, 0)

turnLeft :: MovementVector -> MovementVector
turnLeft vec
  | vec == moveRight = moveUp
  | vec == moveUp    = moveLeft
  | vec == moveLeft  = moveDown
  | vec == moveDown  = moveRight
  | otherwise        = undefined

nextStepIncrement' :: MovementVector -> Int
nextStepIncrement' vec
  | vec == moveUp || vec == moveDown = 1
  | otherwise                        = 0

addCoord :: Coord -> Coord -> Coord
addCoord (y1, x1) (y2, x2) = (y1 + y2, x1 + x2)

distance :: Coord -> Coord -> Int
distance (y1, x1) (y2, x2) = (abs $ y1 - y2) + (abs $ x1 - x2)

isAdjacent :: Coord -> Coord -> Bool
isAdjacent (y1, x1) (y2, x2) = abs (y1 - y2) <= 1 && abs (x1 - x2) <= 1


generateUlamSpiral :: [Coord]
generateUlamSpiral = scanl addCoord (0, 0) (generate' 0 0 moveRight)
  where
    generate' 0 prevStepCount vec = vec : generate' nextSteps' nextSteps' (turnLeft vec)
      where
        nextSteps' = prevStepCount + nextStepIncrement' vec

    generate' s prevStepCount vec = vec : generate' (s - 1) prevStepCount vec

-- Part 1
ulamDistance :: Int -> Int
ulamDistance n = distance (0, 0) (last . take n $ generateUlamSpiral)

-- Part 2
ulamSum :: Int -> Int
ulamSum 0 = 1
ulamSum n = sum $ map (ulamSum . fst) localNeighbors
  where
    spiral = take (n + 1) generateUlamSpiral
    curPos = last spiral
    prevCoords = init spiral

    localNeighbors = filter (\(n, coord) -> isAdjacent coord curPos) (zip [0..] prevCoords)

findNextOver :: Int -> Int
findNextOver n = find' 0 n
  where
    find' i target
      | pointSum > target = pointSum
      | otherwise         = find' (succ i) target

      where
        pointSum = ulamSum i

main :: IO ()
main = do
  putStrLn $ "Part 1: " ++ (show $ ulamDistance puzzleInput)
  putStrLn $ "Part 2: " ++ (show $ findNextOver puzzleInput)
