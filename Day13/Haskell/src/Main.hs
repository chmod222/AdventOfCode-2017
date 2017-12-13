module Main where

import Data.List

data Layer = Layer Int Int Int
    deriving (Show)

layerIndex :: Layer -> Int
layerIndex (Layer idx _ _) = idx

layerInit :: Layer -> Int
layerInit (Layer _ init _) = init

layerCurrent :: Layer -> Int
layerCurrent (Layer _ _ current) = current

bounce :: Int -> Int -> Int
bounce 0 n = 0
bounce h n
    | n `rem` (h * 2) >= h = h - (n `rem` h)
    | otherwise            = n `rem` h

readInput :: String -> Layer
readInput line = Layer layer depth 0
  where
    (layer', depth') = break (== ':') line
    layer = read layer'
    depth = (subtract 1) . read . drop 2 $ depth'

buildFirewall :: [Layer] -> [Layer]
buildFirewall layers = map fn [0 .. layerIndex $ maximumBy (\a b -> compare (layerIndex a) (layerIndex b)) layers]
  where
    fn idx = Layer idx (maybe 0 layerInit (find (\l -> idx == layerIndex l) layers)) 0

layerState :: Layer -> Int -> Layer
layerState (Layer i s c) step = Layer i s (bounce s step)

firewallState :: [Layer] -> Int -> [Layer]
firewallState layers step = map (\l -> layerState l step) layers

layerAt :: [Layer] -> Int -> Layer
layerAt fw time = (firewallState fw time) !! time

trail :: Int -> [Layer] -> [Layer]
trail delay fw = zipWith (\t l -> layerState l (t + delay)) [0 .. length fw] fw

caughtAt :: [Layer] -> [Layer]
caughtAt = filter (\(Layer i s c) -> s > 0 && c == 0)

severity :: [Layer] -> Int
severity = foldr (\l1 acc -> acc + (layerIndex l1) * (layerInit l1 + 1)) 0

findMinimumSafeDelay :: [Layer] -> Int
findMinimumSafeDelay = applyBruteForce 0
  where
    applyBruteForce d fw
        | (length . caughtAt . trail d) fw == 0 = d
        | otherwise                             = applyBruteForce (succ d) fw

main :: IO ()
main = do
    input <- fmap (map readInput . lines) getContents

    let fw = buildFirewall input
    let part1 = severity . caughtAt . trail 0 $ fw

    putStrLn $ "Part 1: " ++ (show part1)
    putStrLn $ "Part 2: " ++ (show $ findMinimumSafeDelay fw)
