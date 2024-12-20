module N20 (getSolutions20) where

import Control.Arrow
import Control.Monad ((>=>))
import Data.Array ((!), (//))
import qualified Data.Array as A
import Data.Foldable (find)
import Data.Maybe (fromJust)
import GraphUtils (ArrayGraph, DijkstraState (distanceMap), Distance (Dist, Inf), DistanceMap, bestPaths, distanceMap, distanceToInt, runDijkstraST)
import Useful (CharGrid, GridPos, countIf, neighbors4, saveGridToFile, strToCharGrid, wordsWhen)

parseFile :: String -> (CharGrid, GridPos, GridPos)
parseFile file = (grid, startPos, endPos)
 where
  grid = strToCharGrid file
  confidentSearch c = fst . fromJust $ find ((c ==) . snd) $ A.assocs grid
  startPos = confidentSearch 'S'
  endPos = confidentSearch 'E'

makeGraph :: CharGrid -> ArrayGraph GridPos
makeGraph grid = A.array bounds edgeAssocs
 where
  bounds = A.bounds grid
  positions = A.indices grid
  edgeAssocs = makeEdges <$> positions
  makeEdges pos = (pos, [(nei, 1) | nei <- neighbors4 pos, valid nei])
  valid pos' = A.inRange bounds pos' && grid ! pos' /= '#'

addDists :: Distance -> Distance -> Distance
addDists (Dist a) (Dist b) = Dist (a + b)
addDists _ _ = Inf

solutionForNs :: Int -> (CharGrid, GridPos, GridPos) -> Int
solutionForNs nanosecs (grid, start, end) = countIf (>= 100) [distanceToInt startToEndDist - cheatCost | Dist cheatCost <- cheatCosts]
 where
  startToEndDist = distMap ! end
  cheatCosts =
    [ addDists (Dist taxicabDist) $ addDists (distMap ! p1) (revDistMap ! p2)
    | p1 <- freeSpaces
    , (p2, taxicabDist) <- taxicabNeighbors nanosecs p1
    , A.inRange bounds p2
    , taxicabDist >= 2 && taxicabDist <= 20
    ]
  bounds = A.bounds grid
  distMap = distanceMap $ runDijkstraST graph start [end]
  revDistMap = distanceMap $ runDijkstraST graph end [start]
  freeSpaces = fst <$> filter (('#' /=) . snd) (A.assocs grid)
  graph = makeGraph grid

taxicab :: GridPos -> GridPos -> Int
taxicab (y, x) (y', x') = abs (y - y') + abs (x - x')
taxicabNeighbors :: Int -> GridPos -> [(GridPos, Int)]
taxicabNeighbors n (y, x) = [((y', x'), taxiDist) | y' <- [y - n .. y + n], x' <- [x - n .. x + n], let taxiDist = taxicab (y', x') (y, x), taxiDist <= n]

solution1 = solutionForNs 2
solution2 = solutionForNs 20
getSolutions20 :: String -> IO (Int, Int)
getSolutions20 = readFile >=> (parseFile >>> (solution1 &&& solution2) >>> return)
