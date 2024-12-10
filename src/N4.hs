module N4 (getSolutions4) where

import Control.Arrow
import Control.Monad ((>=>))
import Data.Array.Unboxed ((!))
import qualified Data.Array.Unboxed as A

import Useful (countIf, CharGridU, strToCharGrid) -- countIf p ls = length $ filter p ls

type Position = (Int, Int)

findAllXmas :: CharGridU -> Position -> Int
findAllXmas grid pos = if grid ! pos /= 'X' then 0 else countIf (isMas grid pos) [(ystep, xstep) | ystep <- [-1 .. 1], xstep <- [-1 .. 1], (ystep, xstep) /= (0, 0)]

isMas :: CharGridU -> Position -> (Int, Int) -> Bool
isMas grid pos (dy, dx) =
  let
    isInRange = A.inRange bounds $ incPosition (3 * dy, 3 * dx) pos
    bounds = A.bounds grid
    posList = tail . take 4 $ iterate (incPosition (dy, dx)) pos
    incPosition (dy, dx) (y, x) = (y + dy, x + dx)
    word = map (grid !) posList
   in
    isInRange && word == "MAS"

isX'MAS :: CharGridU -> Position -> Bool
isX'MAS grid pos@(y, x) = grid ! pos == 'A' && not outOfBounds && matches pair1 && matches pair2
 where
  pair1 = (grid ! (y - 1, x - 1), grid ! (y + 1, x + 1))
  pair2 = (grid ! (y - 1, x + 1), grid ! (y + 1, x - 1))
  matches (v1, v2) = v1 == 'M' && v2 == 'S' || v1 == 'S' && v2 == 'M'
  outOfBounds = not $ A.inRange (xmin + 1, xmax - 1) x && A.inRange (ymin + 1, ymax - 1) y
  ((ymin, xmin), (ymax, xmax)) = A.bounds grid

solution1 :: CharGridU -> Int
solution1 grid = sum $ findAllXmas grid <$> A.indices grid

solution2 :: CharGridU-> Int
solution2 grid = countIf (isX'MAS grid) $ A.indices grid

getSolutions4 :: String -> IO (Int, Int)
getSolutions4 = readFile >=> (strToCharGrid >>> (solution1 &&& solution2) >>> return)

-- >>> getSolutions4 "inputs/4.txt"
-- <stderr>: hPutChar: invalid argument (cannot encode character '\8216')
