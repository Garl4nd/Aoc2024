{-# LANGUAGE NamedFieldPuns #-}
module N17
    (getSolutions17) where
import Control.Arrow
import Control.Monad ((>=>))
import Data.Bits 
import Debugging 
import Data.List (intercalate)

data Computer = Computer {regA :: Int, regB :: Int, regC :: Int, output :: [Int], instPtr :: Int } deriving Show
data Instruction = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv  
type Operand = Int 
type Input = [Int]

opToCombo :: Computer -> Operand -> Int 
opToCombo comp@Computer{regA, regB, regC, output} oper 
  | oper <=3 = oper 
  | oper ==4 = regA 
  | oper ==5 = regB
  | oper ==6 = regC
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


trace :: Show a => String -> a -> a
trace = traceWInfo False

processInput :: Computer -> [Int]  ->  Computer
processInput comp@Computer{regA, regB, regC, output, instPtr} input 
  | [] <- remInput = comp 
  | [_] <- remInput = comp 
  | otherwise  = processInput (trace "res" comp') input  where 
      comp' =   case inst of 
          Adv -> movePtr comp{regA = divRes} 
          Bxl -> movePtr comp {regB = regB `xor`  oper}
          Bst -> movePtr comp {regB = combo `mod` 8}  
          Jnz -> if regA == 0 then movePtr comp else jumpPtr comp oper 
          Bxc -> movePtr comp {regB = regB `xor` regC}
          Out -> movePtr comp{output= output ++ [combo `mod` 8]}
          Bdv -> movePtr comp{regB = divRes}
          Cdv -> movePtr comp{regC = divRes}
      remInput = drop instPtr input 
      instNum:oper:_ = trace "reminput" remInput
      inst = intToInstruction instNum 
      combo = opToCombo comp oper
      divRes = regA `div` (2^combo)            
      jumpPtr c inc  = c{instPtr = inc} 
      movePtr c = jumpPtr c (instPtr+2)

    

getSolutions17 :: String -> IO (Int, Int)
getSolutions17 =  const $ return (0,0) -- readFile >=> (parseFile >>> (solution1 &&& solution2) >>> return)

regA_:: Int
regB_ :: Int 
regC_ :: Int


regA_ = 117440
regB_ = 0
regC_ = 0

input :: [Int]
input =  [2,4,1,5,7,5,1,6,0,3,4,0,5,5,3,0]

solution1 :: String 
solution1 =  intercalate "," . map show . output $  processInput  Computer{regA = regA_, regB = regB_, regC = regC_, output = [], instPtr = 0} input

-- The adv instruction (opcode 0) performs division. The numerator is the value in the A register. The denominator is found by raising 2 to the power of the instruction's combo operand. (So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.) The result of the division operation is truncated to an integer and then written to the A register.

-- The bxl instruction (opcode 1) calculates the bitwise XOR of register B and the instruction's literal operand, then stores the result in register B.
-- The bst instruction (opcode 2) calculates the value of its combo operand modulo 8 (thereby keeping only its lowest 3 bits), then writes that value to the B register.

-- The jnz instruction (opcode 3) does nothing if the A register is 0. However, if the A register is not zero, it jumps by setting the instruction pointer to the value of its literal operand; if this instruction jumps, the instruction pointer is not increased by 2 after this instruction.

-- The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C, then stores the result in register B. (For legacy reasons, this instruction reads an operand but ignores it.)

-- The out instruction (opcode 5) calculates the value of its combo operand modulo 8, then outputs that value. (If a program outputs multiple values, they are separated by commas.)

-- The bdv instruction (opcode 6) works exactly like the adv instruction except that the result is stored in the B register. (The numerator is still read from the A register.)

-- The cdv instruction (opcode 7) works exactly like the adv instruction except that the result is stored in the C register. (The numerator is still read from the A register.)