{-# LANGUAGE NamedFieldPuns #-}

module N20 (getSolutions20) where

import Control.Arrow
import Control.Monad ((>=>))
import Data.Array ((!), (//))
import qualified Data.Array as A
import Data.Foldable (find)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import GraphUtils (ArrayGraph, DijkstraState (distanceMap), Distance (Dist, Inf), DistanceMap, bestPaths, distanceMap, distanceToInt, runDijkstraST)
import Useful (CharGrid, GridPos, neighbors4, saveGridToFile, strToCharGrid, wordsWhen)

type AugPos = (GridPos, Int)

parseFile :: String -> (CharGrid, GridPos, GridPos)
parseFile file = (grid, startPos, endPos)
 where
  grid = strToCharGrid file
  confidentSearch c = fst . fromJust $ find ((c ==) . snd) $ A.assocs grid
  startPos = confidentSearch 'S'
  endPos = confidentSearch 'E'

dims :: (Int, Int)
dims = (70, 70)

makeGraph :: CharGrid -> [GridPos] -> Int -> ArrayGraph AugPos
makeGraph charGrid cheatableGrids maxCheats = A.array bounds edgeAssocs
 where
  posBounds@(minPos, maxPos) = A.bounds charGrid
  bounds = ((minPos, 0), (maxPos, 1))
  edgeAssocs = makeEdges <$> augIndices
  augIndices = [(pos, cheats) | pos <- A.indices charGrid, cheats <- [0 .. maxCheats]]
  makeEdges augPos@(_, cheats) = (augPos, if cheats == 0 then regularEdges augPos else regularEdges augPos ++ cheatEdges augPos)
  regularEdges (pos, cheats) = [((nei, cheats), 1) | nei <- neighbors4 pos, regularValid nei]
  cheatEdges (pos, cheats) = [((nei, cheats - 1), 1) | nei <- neighbors4 pos, cheatValid nei]
  regularValid pos' = A.inRange posBounds pos' && charGrid ! pos' /= '#'
  cheatValid pos' = A.inRange posBounds pos' && pos' `elem` cheatableGrids

addDists :: Distance -> Distance -> Distance
addDists (Dist a) (Dist b) = Dist (a + b)
addDists _ _ = Inf

type Path = [GridPos]
solution1' :: (CharGrid, GridPos, GridPos) -> [Int]
solution1' (grid, start, end) = filter (> 0) [distanceToInt regularCost - cheatCost | Dist cheatCost <- cheatCosts]
 where
  regularCost = distMapReg ! (end, 0)
  cheatCosts = [addDists (distMapS ! (barrier, 0)) (distMapE ! (barrier, 0)) | barrier <- barriers]
  distMapS :: A.Array AugPos Distance
  distMapS =
    distanceMap $
      runDijkstraST
        (graph barriers)
        (start, 1)
        [(pos, 0) | pos <- barriers]
  distMapE :: A.Array AugPos Distance
  distMapE =
    distanceMap $
      runDijkstraST
        (graph barriers)
        (end, 1)
        [(pos, 0) | pos <- barriers]
  distMapReg :: A.Array AugPos Distance
  distMapReg = distanceMap $ runDijkstraST (graph []) (start, 0) [(end, 0)]
  barriers = fst <$> filter (('#' ==) . snd) (A.assocs grid)
  graph cheatablePositions = makeGraph grid cheatablePositions (if null cheatablePositions then 0 else 1)
  augEnd = (end, 0)

solution2 :: (CharGrid, GridPos, GridPos) -> [Int]
solution2 (grid, start, end) = filter (>= 100) [distanceToInt regularCost - cheatCost | Dist cheatCost <- cheatCosts]
 where
  regularCost = distMapReg ! (end, 0)
  cheatCosts =
    [ addDists (Dist taxicabDist) $ addDists (distMapS ! (p1, 0)) (distMapE ! (p2, 0))
    | p1 <- freeSpaces
    , (p2, taxicabDist) <- taxicabNeighbors 20 p1
    , A.inRange bounds p2
    , taxicabDist <= 20
    ]
  bounds = A.bounds grid
  distMapS :: A.Array AugPos Distance
  distMapS =
    distanceMap $
      runDijkstraST
        (graph barriers)
        (start, 0)
        [(pos, 0) | pos <- freeSpaces]
  distMapE :: A.Array AugPos Distance
  distMapE =
    distanceMap $
      runDijkstraST
        (graph barriers)
        (end, 0)
        [(pos, 0) | pos <- freeSpaces]
  distMapReg :: A.Array AugPos Distance
  distMapReg = distanceMap $ runDijkstraST (graph []) (start, 0) [(end, 0)]
  barriers = fst <$> filter (('#' ==) . snd) (A.assocs grid)
  freeSpaces = fst <$> filter (('#' /=) . snd) (A.assocs grid)

  graph cheatablePositions = makeGraph grid cheatablePositions (if null cheatablePositions then 0 else 1)
  augEnd = (end, 0)

taxicab :: GridPos -> GridPos -> Int
taxicab (y, x) (y', x') = abs (y - y') + abs (x - x')
taxicabNeighbors :: Int -> GridPos -> [(GridPos, Int)]
taxicabNeighbors n (y, x) = [((y', x'), taxiDist) | y' <- [y - n .. y + n], x' <- [x - n .. x + n], let taxiDist = taxicab (y', x') (y, x), taxiDist <= n]
solution1 :: (CharGrid, GridPos, GridPos) -> [Int]
solution1 (grid, start, end) = filter (> 0) [distanceToInt regularCost - cheatCost | Dist cheatCost <- cheatCosts]
 where
  regularCost = distMap [] ! (end, 0)
  cheatCosts = [distMap [barrier] ! augEnd | barrier <- barriers]
  distMap :: [GridPos] -> A.Array AugPos Distance
  distMap cheatablePositions = distanceMap $ runDijkstraST (graph cheatablePositions) (start, if null cheatablePositions then 0 else 1) [augEnd]
  barriers = fst <$> filter (('#' ==) . snd) (A.assocs grid)
  graph cheatablePositions = makeGraph grid cheatablePositions (if null cheatablePositions then 0 else 1)
  augEnd = (end, 0)

-- >>> solution2 . parseFile <$> readFile "inputs/18.txt"
-- <stderr>: hPutChar: invalid argument (cannot encode character '\8216')

getSolutions20 :: String -> IO (Int, Int)
getSolutions20 = const $ return (0, 0) -- readFile >=> (parseFile >>> (solution1 &&& solution2) >>> return)
