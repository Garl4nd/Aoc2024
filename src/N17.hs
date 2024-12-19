{-# LANGUAGE NamedFieldPuns #-}

module N17 (getSolutions17) where

import Data.Bits

data Computer = Computer {regA :: Int, regB :: Int, regC :: Int, output :: [Int], instPtr :: Int} deriving (Show)
data Instruction = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv
type Operand = Int

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

processInput :: Computer -> [Int] -> Computer
processInput comp0 input = go comp0
 where
  go :: Computer -> Computer
  go comp@Computer{regA, regB, regC, output, instPtr}
    | [] <- remInput = comp
    | [_] <- remInput = comp
    | otherwise = go comp'
   where
    comp' = case inst of
      Adv -> movePtr comp{regA = divRes}
      Bxl -> movePtr comp{regB = regB `xor` oper}
      Bst -> movePtr comp{regB = combo `mod` 8}
      Jnz -> if regA == 0 then movePtr comp else jumpPtr comp oper
      Bxc -> movePtr comp{regB = regB `xor` regC}
      Out -> movePtr comp{output = output ++ [combo `mod` 8]}
      Bdv -> movePtr comp{regB = divRes}
      Cdv -> movePtr comp{regC = divRes}
    remInput = drop instPtr input
    instNum : oper : _ = remInput
    inst = intToInstruction instNum
    combo = opToCombo comp oper
    divRes = shiftR regA combo
    jumpPtr c inc = c{instPtr = inc}
    movePtr c = jumpPtr c (instPtr + 2)

-- problemInput = []
getResultFromA :: [Int] -> Int -> [Int]
getResultFromA input a = output $ processInput Computer{regA = a, regB = 0, regC = 0, output = [], instPtr = 0} input

solution1 :: [Int] -> Int
solution1 input = read $ concatMap show $ getResultFromA input 24847151

firstOutput :: [Int] -> Int -> Int
firstOutput = (head .) . getResultFromA

growPossibleARegs :: [Int] -> Int -> Int -> [Int]
growPossibleARegs input b a = filter ((b `mod` 8 ==) . firstOutput input) [8 * a .. 8 * a + 7]

solution2 :: [Int] -> Int
solution2 input = minimum . foldr (concatMap . growPossibleARegs input) [0] $ input

getSolutions17 :: String -> IO (Int, Int)
getSolutions17 = const $ return (solution1 problemInput, solution2 problemInput)

problemInput :: [Int]
problemInput = [] --  provide actual input, didnt bother parsing the file this time

-- The adv instruction (opcode 0) performs division. The numerator is the value in the A register. The denominator is found by raising 2 to the power of the instruction's combo operand. (So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.) The result of the division operation is truncated to an integer and then written to the A register.

-- The bxl instruction (opcode 1) calculates the bitwise XOR of register B and the instruction's literal operand, then stores the result in register B.
-- The bst instruction (opcode 2) calculates the value of its combo operand modulo 8 (thereby keeping only its lowest 3 bits), then writes that value to the B register.

-- The jnz instruction (opcode 3) does nothing if the A register is 0. However, if the A register is not zero, it jumps by setting the instruction pointer to the value of its literal operand; if this instruction jumps, the instruction pointer is not increased by 2 after this instruction.

-- The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C, then stores the result in register B. (For legacy reasons, this instruction reads an operand but ignores it.)

-- The out instruction (opcode 5) calculates the value of its combo operand modulo 8, then outputs that value. (If a program outputs multiple values, they are separated by commas.)

-- The bdv instruction (opcode 6) works exactly like the adv instruction except that the result is stored in the B register. (The numerator is still read from the A register.)

-- The cdv instruction (opcode 7) works exactly like the adv instruction except that the result is stored in the C register. (The numerator is still read from the A register.)
