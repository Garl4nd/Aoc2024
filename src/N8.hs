module N8 (getSolutions8) where

import Control.Arrow
import Control.Monad ((>=>))
import qualified Data.Array.Unboxed as A
import qualified Data.Map as M
import qualified Data.Set as S
import Useful (CharGrid, GridPos, pairVariations, strToCharGrid)

type Frequency = Char
type FrequencyMap = M.Map Frequency [GridPos]
type AntinodeGenerator = ((GridPos, GridPos) -> [GridPos])

getFrequencyMap :: CharGrid -> FrequencyMap
getFrequencyMap charGrid =
  let antennas = filter ((/= '.') . snd) $ A.assocs charGrid
      mapAssocs = [(val, [pos]) | (pos, val) <- antennas]
   in M.fromListWith (++) mapAssocs

solveWith :: AntinodeGenerator -> CharGrid -> Int
solveWith antinodeGenerator charGrid = length $ S.fromList . concatMap antinodesForFreq $ M.elems freqMap
 where
  antinodesForFreq :: [GridPos] -> [GridPos]
  antinodesForFreq = concatMap (takeWhile (A.inRange bounds) . antinodeGenerator) . pairVariations
  bounds = A.bounds charGrid
  freqMap = getFrequencyMap charGrid

antinodeGen1 = take 1 . drop 1 . antinodeGen2 -- ((y1, x1), (y2, x2)) = [(y1 + (y1 - y2), x1 + (x1 - x2))]
antinodeGen2 ((y1, x1), (y2, x2)) = [(y1 + k * (y1 - y2), x1 + k * (x1 - x2)) | k <- [0 ..]]

getSolutions8 :: String -> IO (Int, Int)
getSolutions8 = readFile >=> (strToCharGrid >>> (solveWith antinodeGen1 &&& solveWith antinodeGen2) >>> return)
