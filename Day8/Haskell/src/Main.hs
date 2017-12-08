module Main where

import Data.Maybe

import qualified Data.Set as S
import qualified Data.Map as M

type Register = String

data Instruction a = Inc Register a
                   | Dec Register a
    deriving (Show)

getRegister :: (Num a) => Instruction a -> String
getRegister (Inc r _) = r
getRegister (Dec r _) = r

data Condition a = Gt Register a
                 | Ge Register a
                 | Lt Register a
                 | Le Register a
                 | Eq Register a
                 | Neq Register a
    deriving (Show)

type FullInstruction a = (Instruction a, Condition a)
type RegisterMap a = M.Map String a

parseInstruction :: (Read a, Num a) => String -> FullInstruction a
parseInstruction str = case words str of
    (r : "inc" : n : cond) -> (Inc r (read n), parseCondition cond)
    (r : "dec" : n : cond) -> (Dec r (read n), parseCondition cond)
    _                      -> undefined

  where
    parseCondition ("if" : reg : ">" : n : []) = (Gt reg (read n))
    parseCondition ("if" : reg : ">=" : n : []) = (Ge reg (read n))
    parseCondition ("if" : reg : "<" : n : []) = (Lt reg (read n))
    parseCondition ("if" : reg : "<=" : n : []) = (Le reg (read n))
    parseCondition ("if" : reg : "==" : n : []) = (Eq reg (read n))
    parseCondition ("if" : reg : "!=" : n : []) = (Neq reg (read n))
    parseCondition e = error $ "bad condition" ++ unwords e

evalCondition :: (Ord a, Eq a) => Condition a -> RegisterMap a -> Bool
evalCondition (Gt reg v) regs = ((fromJust . M.lookup reg) regs) > v
evalCondition (Ge reg v) regs = ((fromJust . M.lookup reg) regs) >= v
evalCondition (Lt reg v) regs = ((fromJust . M.lookup reg) regs) < v
evalCondition (Le reg v) regs = ((fromJust . M.lookup reg) regs) <= v
evalCondition (Eq reg v) regs = ((fromJust . M.lookup reg) regs) == v
evalCondition (Neq reg v) regs = ((fromJust . M.lookup reg) regs) /= v

applyInstr :: (Num a) => RegisterMap a -> Instruction a -> RegisterMap a
applyInstr regs (Inc reg v) = M.update (Just . (+ v)) reg regs
applyInstr regs (Dec reg v) = M.update (Just . (subtract v)) reg regs

runProgram :: [FullInstruction Int] -> RegisterMap Int -> [RegisterMap Int]
runProgram [] regs = []
runProgram ((ii, ic):is) regs
    | evalCondition ic regs = newState : runProgram is newState
    | otherwise             = regs : runProgram is regs
  where
    newState = applyInstr regs ii

main :: IO ()
main = do
    input <- fmap (map parseInstruction . lines) getContents :: IO [FullInstruction Int]
    let registers = S.fromList [getRegister ins | ins <- map fst input]
    let registerInit = M.fromList [(reg, 0) | reg <- S.toList registers]

    let allStates = runProgram input registerInit

    let maxValue = maximum (last allStates)
    let absoluteMaxValue = maximum (map maximum allStates)

    putStrLn $ "Part 1: " ++ (show maxValue)
    putStrLn $ "Part 2: " ++ (show absoluteMaxValue)

