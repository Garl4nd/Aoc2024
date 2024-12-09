{-# LANGUAGE NamedFieldPuns #-}
module N9 () where

import Control.Arrow
import Control.Monad ((>=>))
import Prelude hiding (id)
import qualified Data.Array.Unboxed as A
import Useful (consecutivePairs)
import Data.Maybe (fromJust)
import Data.Char (digitToInt)

type ID = Int
data Block = FilledBlock {id :: ID, size :: Int} | FreeBlock deriving (Show)
type Disk = [Block]

parseFile :: String -> Disk 
parseFile file = fillDisk 0 $ digitToInt <$> numberList where
  numberList = init file
  fillDisk :: ID -> [Int] -> Disk
  fillDisk id [] = []
  fillDisk id [size] = [FilledBlock {id, size =size}]
  fillDisk id (filledSize:freeSize:rest) = FilledBlock {id, size= filledSize} : replicate freeSize FreeBlock ++ fillDisk (id+1) rest
  --(blocks, freeBlocks) = unzip . fromJust . consecutivePairs . map digitToInt . init  $ numberList

isFree :: Block -> Bool
isFree FreeBlock = True
isFree _         = False

moveLastBlock :: Disk -> Disk 
moveLastBlock disk = let 
  lastBlock = last $ filter (not . isFree)  disk
  in disk

-- moveBlock ::  Disk -> Disk 
-- moveBlock (blocks, freeBlocks)= let 
--   lastBlock@Block{id, size} = last blocks
--   fillBlocks  

-- >>> sum parseFile <$> readFile "C:\\Users\\filip.kostka\\Personal\\Aoc2024\\inputs/9.txt" 

-- getSolutions8 :: String -> IO (Int, Int)
-- getSolutions8 = readFile >=> (strToCharGrid >>> (solveWith antinodeGen1 &&& solveWith antinodeGen2) >>> return)
