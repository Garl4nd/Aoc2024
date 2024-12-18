{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module N17 (getSolutions17) where

import Control.Arrow
import Control.Monad ((>=>))
import Control.Monad.Fix (fix)
import Data.Bits
import Data.Function.Memoize
import Data.List (intercalate, tails, zip5)
import Data.MemoTrie
import Debugging
import N1 (getSolutions1)

type Memo f = f -> f

data Computer = Computer {regA :: Int, regB :: Int, regC :: Int, output :: [Int], instPtr :: Int} deriving (Show)
data Instruction = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv
type Operand = Int
type Input = [Int]

opToCombo :: Computer -> Operand -> Int
opToCombo comp@Computer{regA, regB, regC, output} oper
  | oper <= 3 = oper
  | oper == 4 = regA
  | oper == 5 = regB
  | oper == 6 = regC
  | otherwise = error "Invalid opcode"

intToInstruction :: Int -> Instruction
intToInstruction n = case n of
  0 -> Adv
  1 -> Bxl
  2 -> Bst
  3 -> Jnz
  4 -> Bxc
  5 -> Out
  6 -> Bdv
  7 -> Cdv

trace :: (Show a) => String -> a -> a
trace = traceWInfo True

processInput :: Computer -> [Int] -> Computer
processInput comp0 input = go comp0
 where
  go :: Computer -> Computer
  go comp@Computer{regA, regB, regC, output, instPtr}
    | [] <- remInput = comp
    | [_] <- remInput = comp
    | otherwise = go (trace "res" comp')
   where
    comp' = case instNum of
      -- Adv -> movePtr comp{regA = divRes}
      -- Bxl -> movePtr comp{regB = regB `xor` oper}
      -- Bst -> movePtr comp{regB = combo `mod` 8}
      -- Jnz -> if regA == 0 then movePtr comp else jumpPtr comp oper
      -- Bxc -> movePtr comp{regB = regB `xor` regC}
      -- Out -> movePtr comp{output = output ++ [combo `mod` 8]}
      -- Bdv -> movePtr comp{regB = divRes}
      -- Cdv -> movePtr comp{regC = divRes}
      0 -> movePtr comp{regA = divRes}
      1 -> movePtr comp{regB = regB `xor` oper}
      2 -> movePtr comp{regB = combo `mod` 8}
      3 -> if regA == 0 then movePtr comp else jumpPtr comp oper
      4 -> movePtr comp{regB = regB `xor` regC}
      5 -> movePtr comp{output = output ++ [combo `mod` 8]}
      6 -> movePtr comp{regB = divRes}
      7 -> movePtr comp{regC = divRes}

    remInput = remInputs !! instPtr -- drop instPtr input
    instNum : oper : _ = trace "reminput" remInput
    inst = intToInstruction instNum
    combo = opToCombo comp oper
    divRes = shiftR regA combo
    jumpPtr c inc = c{instPtr = inc}
    movePtr c = jumpPtr c (instPtr + 2)
  remInputs = tails input

type Registers = (Int, Int, Int)

opToCombo' :: Registers -> Int -> Int
opToCombo' (regA, regB, regC) oper
  | oper <= 3 = oper
  | oper == 4 = regA
  | oper == 5 = regB
  | oper == 6 = regC
  | otherwise = 0 -- error "Invalid opcode"

processInput' :: Registers -> [Int] -> [Int]
processInput' (regA, regB, regC) input = go (regA, regB, regC) 0
 where
  -- goM = memoFix2 go
  go :: Registers -> Int -> [Int]
  go registers@(regA, regB, regC) offset
    | offset >= length input - 1 = []
    | otherwise = output' ++ go (regA', regB', regC') offset'
   where
    (regA', regB', regC', output', offset') = case instNum of
      0 -> (divRes, regB, regC, [], offset + 2)
      1 -> (regA, regB `xor` oper, regC, [], offset + 2)
      2 -> (regA, combo `mod` 8, regC, [], offset + 2)
      3 -> (regA, regB, regC, [], if regA == 0 then offset + 2 else 0)
      4 -> (regA, regB `xor` regC, regC, [], offset + 2)
      5 -> (regA, regB, regC, [combo `mod` 8], offset + 2)
      6 -> (regA, divRes, regC, [], offset + 2)
      7 -> (regA, regB, divRes, [], offset + 2)
    remInput = remInputs !! offset
    instNum : oper : _ = remInput
    combo = opToCombo' registers oper
    divRes = divResM regA combo
    divResM = divResF
    divResF x y = x `div` (2 ^ y)
  remInputs = tails input

regA_ :: Int
regB_ :: Int
regC_ :: Int
regA_ = 24847151
regB_ = 0
regC_ = 0

problemInput :: [Int]
problemInput = [2, 4, 1, 5, 7, 5, 1, 6, 0, 3, 4, 0, 5, 5, 3, 0]

solution1 :: Input -> Int
solution1 input = read $ concatMap show . output $ (processInput) Computer{regA = regA_, regB = regB_, regC = regC_, output = [], instPtr = 0} input

solution1' :: Input -> String
solution1' input = intercalate "," . map show $ processInput' (regA_, regB_, regC_) input

getResultForA :: [Int] -> Int -> [Int]
getResultForA input a = output $ processInputM Computer{regA = a, regB = 0, regC = 0, output = [], instPtr = 0} input
 where
  processInputM = processInput

getResultForA' :: [Int] -> Int -> [Int]
getResultForA' input a = processInput' (a, 0, 0) input

matchingAs :: Int -> Int -> [Int] -> [Int]
matchingAs numA correctPlaces input = take numA . filter (\a -> take correctPlaces (getResultForA input a) == take correctPlaces input) $ [0 ..]

matchingAs' :: Int -> Int -> [Int] -> [Int]
matchingAs' numA correctPlaces input = take numA . filter (\a -> take correctPlaces (getResultForA' input a) == take correctPlaces input) $ [0 ..]

getSolutions17 :: String -> IO (Int, Int)
getSolutions17 _ = do
  print $ matchingAs' 20 5 problemInput
  print $ matchingAs' 20 6 problemInput
  print $ matchingAs' 10 7 problemInput
  print $ matchingAs' 5 8 problemInput
  print $ matchingAs' 5 9 problemInput
  print $ matchingAs' 5 10 problemInput
  return (solution1 problemInput, 0)

-- The adv instruction (opcode 0) performs division. The numerator is the value in the A register. The denominator is found by raising 2 to the power of the instruction's combo operand. (So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.) The result of the division operation is truncated to an integer and then written to the A register.

-- The bxl instruction (opcode 1) calculates the bitwise XOR of register B and the instruction's literal operand, then stores the result in register B.
-- The bst instruction (opcode 2) calculates the value of its combo operand modulo 8 (thereby keeping only its lowest 3 bits), then writes that value to the B register.

-- The jnz instruction (opcode 3) does nothing if the A register is 0. However, if the A register is not zero, it jumps by setting the instruction pointer to the value of its literal operand; if this instruction jumps, the instruction pointer is not increased by 2 after this instruction.

-- The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C, then stores the result in register B. (For legacy reasons, this instruction reads an operand but ignores it.)

-- The out instruction (opcode 5) calculates the value of its combo operand modulo 8, then outputs that value. (If a program outputs multiple values, they are separated by commas.)

-- The bdv instruction (opcode 6) works exactly like the adv instruction except that the result is stored in the B register. (The numerator is still read from the A register.)

-- The cdv instruction (opcode 7) works exactly like the adv instruction except that the result is stored in the C register. (The numerator is still read from the A register.)
