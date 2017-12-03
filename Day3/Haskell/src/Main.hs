module Main (main, generateUlamSpiral) where

import Prelude hiding (Left, Right)
import Data.List

type Coord = (Int, Int)

puzzleInput :: Int
puzzleInput = 361527

data Direction = Right
               | Up
               | Left
               | Down
  deriving (Show, Read)

nextDir :: Direction -> Direction
nextDir Right = Up
nextDir Up    = Left
nextDir Left  = Down
nextDir Down  = Right

nextStepIncrement :: Direction -> Int
nextStepIncrement Up   = 1
nextStepIncrement Down = 1
nextStepIncrement _    = 0

coordStep :: Direction -> Coord
coordStep Right = (0, 1)
coordStep Up    = (-1, 0)
coordStep Left  = (0, -1)
coordStep Down  = (1, 0)

addCoord :: Coord -> Coord -> Coord
addCoord (y1, x1) (y2, x2) = (y1 + y2, x1 + x2)

distance :: Coord -> Coord -> Int
distance (y1, x1) (y2, x2) = (abs $ y1 - y2) + (abs $ x1 - x2)

isAdjacent :: Coord -> Coord -> Bool
isAdjacent (y1, x1) (y2, x2) = abs (y1 - y2) <= 1 && abs (x1 - x2) <= 1

generateUlamSpiral :: [Direction]
generateUlamSpiral = generate' Right 0 0
  where
    generate' dir 0 prevStepCount = dir : generate' nextDirection nextSteps nextSteps
      where
        nextSteps = prevStepCount + nextStepIncrement dir
        nextDirection = nextDir dir

    generate' dir s prevStepCount = dir : generate' dir (s - 1) prevStepCount


buildCoordinates :: [Direction] -> (Int, Int) -> (Coord, [Coord])
buildCoordinates spiral center = (finalCoord, center : coords)
  where
    (finalCoord, coords) = mapAccumL
        (\acc dir -> ( addCoord acc (coordStep dir)
                     , addCoord acc (coordStep dir))) center spiral

-- Part 1
ulamDistance :: Int -> Int
ulamDistance n = distance (0, 0) (fst $ buildCoordinates (take (n - 1) generateUlamSpiral) (0, 0))

-- Part 2
ulamSum :: Int -> Int
ulamSum 0 = 1
ulamSum n = sum $ map (ulamSum . fst) localNeighbors
  where
    (currentPosition', allPrevious') = buildCoordinates (take n generateUlamSpiral) (0, 0)

    localNeighbors = filter (\(n, coord) -> isAdjacent coord currentPosition') (zip [0 .. n-1] allPrevious')

findNextOver :: Int -> Int
findNextOver n = find' 0 n
  where
    find' i target =
      if ulamSum i > target
        then ulamSum i
        else find' (succ i) target

main :: IO ()
main = do
  putStrLn $ "Part 1: " ++ (show $ ulamDistance puzzleInput)
  putStrLn $ "Part 2: " ++ (show $ findNextOver puzzleInput)
