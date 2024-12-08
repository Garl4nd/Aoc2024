module N8 (getSolutions8) where

import Control.Arrow
import Control.Monad ((>=>))
import qualified Data.Array.Unboxed as A
import qualified Data.Map as M
import qualified Data.Set as S
import Useful (CharGrid, GridPos, pairVariations, strToCharGrid) -- type CharGrid = A.UArray (Int, Int) Char

type Frequency = Char
type FrequencyMap = M.Map Frequency [GridPos]
type AntinodeGenerator = ((GridPos, GridPos) -> [GridPos])

frequencyMap :: CharGrid -> M.Map Frequency [GridPos]
frequencyMap charGrid =
  let antennas = filter ((/= '.') . snd) $ A.assocs charGrid
      mapAssocs = [(val, [pos]) | (pos, val) <- antennas]
   in M.fromListWith (++) mapAssocs

getAllAntinodes :: CharGrid -> AntinodeGenerator -> FrequencyMap -> S.Set GridPos
getAllAntinodes charGrid antinodeMaker freqMap = S.fromList . concatMap getAntinodes $ M.elems freqMap
 where
  getAntinodes :: [GridPos] -> [GridPos]
  getAntinodes positions = concatMap (takeWhile (A.inRange bounds) . antinodeMaker) $ pairVariations positions
  bounds = A.bounds charGrid

solveWith :: AntinodeGenerator -> String -> Int
solveWith antinodeFunc file = length $ getAllAntinodes grid antinodeFunc freqMap
 where
  grid = strToCharGrid file
  freqMap = frequencyMap grid

solution1 = solveWith $ \((y1, x1), (y2, x2)) -> [(y1 + (y1 - y2), x1 + (x1 - x2))]
solution2 = solveWith $ \((y1, x1), (y2, x2)) -> [(y1 + k * (y1 - y2), x1 + k * (x1 - x2)) | k <- [0 ..]]

getSolutions8 :: String -> IO (Int, Int)
getSolutions8 = readFile >=> ((solution1 &&& solution2) >>> return)

-- solution2 = solveWIth (\()

-- >>> M.elems <$> findAllAntinodes <$> (frequencyMap . strToCharGrid) <$> readFile "inputs/8_test.txt"
-- [[(6,2),(7,4),(8,1),(3,11),(5,10),(6,7),(1,7),(2,4),(4,3),(-1,13),(0,10),(1,12)],[(11,11),(14,13),(8,8),(12,11),(2,4),(3,5)]]
