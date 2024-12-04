module N3 (getSolutions4) where

import Data.Void (Void)
import Control.Arrow
import Control.Monad ((>=>), void)
import Text.Megaparsec.Debug
import Data.Either (fromRight)
import Data.Maybe ( catMaybes)
import qualified Data.Array as A
import Useful (countIf)
-- import qualified Data.Text as T
type Position = (Int, Int)
type CharGrid = A.Array Position Char

strToCharGrid :: String -> CharGrid
strToCharGrid file = A.listArray ((1,1), (numLines, lineSize)) $ concat ls  where
  ls = lines file
  numLines = length ls
  lineSize = length $ head ls

findAllXmas :: CharGrid -> Position -> Int
findAllXmas grid pos = countIf (isXmas grid pos) [(ystep, xstep) | ystep <- [-1..1], xstep <- [-1..1], (ystep, xstep) /= (0,0)]

isXmas _ _  = const True

getSolutions4 :: String -> IO (Int, Int)
getSolutions4 = const $ return  (0,0)
