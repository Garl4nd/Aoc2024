module N4 (getSolutions4) where

import Control.Arrow
import Control.Monad (void, (>=>))
import Data.Array ((!))
import qualified Data.Array as A
import Data.Either (fromRight)
import Data.Maybe (catMaybes)
import Data.Void (Void)
import Debugging
import Useful (countIf)

-- import qualified Data.Text as T
type Position = (Int, Int)
type CharGrid = A.Array Position Char

strToCharGrid :: String -> CharGrid
strToCharGrid file = A.listArray ((1, 1), (numLines, lineSize)) $ concat ls
 where
  ls = lines file
  numLines = length ls
  lineSize = length $ head ls

findAllXmas :: CharGrid -> Position -> Int
findAllXmas grid pos = if grid ! pos /= 'X' then 0 else countIf (isXmas grid pos) [(ystep, xstep) | ystep <- [-1 .. 1], xstep <- [-1 .. 1], (ystep, xstep) /= (0, 0)]

-- traceWInfoA = traceWInfo True
isXmas :: CharGrid -> Position -> (Int, Int) -> Bool
isXmas grid pos (dy, dx) =
  let
    -- isX = grid ! pos == 'X'
    isInRange = A.inRange bounds $ incPosition (3 * dy, 3 * dx) pos
    bounds = A.bounds grid
    (_ : posList) = take 4 $ iterate (incPosition (dy, dx)) pos
    incPosition (dy, dx) (y, x) = (y + dy, x + dx)
    word = [grid ! (y', x') | (y', x') <- posList]
   in
    isInRange && word == "MAS"

isX'MAS :: CharGrid -> Position -> Bool
isX'MAS grid pos@(y, x) = if grid ! pos /= 'A' || outOfBounds then False else matches ul dr && matches dl ur
 where
  [ul, dl, ur, dr] = (grid !) <$> [(y + dy, x + dx) | dy <- [-1, 1], dx <- [-1, 1]]
  matches p1 p2 = p1 == 'M' && p2 == 'S' || p1 == 'S' && p2 == 'M'
  outOfBounds = not $ A.inRange (xmin + 1, xmax - 1) x && A.inRange (ymin + 1, ymax - 1) y
  ((ymin, xmin), (ymax, xmax)) = A.bounds grid

solution1 :: CharGrid -> Int
solution1 grid = sum $ (findAllXmas grid) <$> A.indices grid

solution2 :: CharGrid -> Int
solution2 grid = countIf (isX'MAS grid) $ A.indices grid

getSolutions4 :: String -> IO (Int, Int)
getSolutions4 = readFile >=> (strToCharGrid >>> (solution1 &&& solution2) >>> return)

-- >>> getSolutions4 "inputs/4.txt"
-- (2551,1985)

-- *** Exception: Error in array index

-- *** Exception: Error in array index

-- *** Exception: Error in array index

-- *** Exception: Error in array index

-- *** Exception: Error in array index

-- *** Exception: inputs.txt: openFile: does not exist (No such file or directory)
