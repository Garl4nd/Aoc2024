{-# LANGUAGE NamedFieldPuns #-}

module N18 (getSolutions18) where
import Control.Monad ((>=>))
import Control.Arrow 
import GraphUtils (runDijkstraST, ArrayGraph, Distance (Dist), distanceToInt, distanceMap, DijkstraState (distanceMap), DistanceMap, bestPaths)
import Useful (CharGrid, wordsWhen, neighbors4, GridPos, saveGridToFile)
import qualified Data.Array as A
import Data.Array ((!), (//))
import Data.Maybe (fromJust)
import Data.Tuple (swap)

parseFile :: String -> [GridPos]
parseFile file = coordsList where
  coordsList = map parseCoords . lines $ file   
  parseCoords  line = let [x,y] = map read . wordsWhen (==',')  $ line in (y,x)

dims :: (Int, Int)
dims = (70,70)

makeGrid :: Int -> [GridPos] ->  CharGrid 
makeGrid n coordsList  =  A.accumArray (const id) '.' ((0, 0), dims) [(coords, '#') | coords <- take (n +1) coordsList]


makeGraph :: CharGrid -> ArrayGraph GridPos
makeGraph charGrid = A.array bounds edgeAssocs where 
  bounds = A.bounds charGrid 
  edgeAssocs = map makeEdges $ A.indices charGrid
  makeEdges pos =  (pos, if not (valid pos) then [] else [ (nei,1) | nei <- neighbors4 pos, valid nei]) 
  valid pos' = A.inRange bounds pos' && charGrid ! pos' /= '#'

type Path = [GridPos]


solveForN :: Int -> [GridPos]  -> Distance
solveForN n coordsList =  distMap ! dims where 
  distMap :: A.Array GridPos Distance 
  distMap = distanceMap $   runDijkstraST graph (0,0) [dims]
  graph = makeGraph . makeGrid n $  coordsList 

solution1 :: [GridPos] -> Int 
solution1  =  distanceToInt . solveForN 1024 

binarySearch :: Show a => (a->Bool) -> [a] -> Maybe a 
binarySearch _ [] = Nothing
binarySearch p [x] = if p x then Just x else Nothing
binarySearch p ls@(s:rest)
  | p s = Just s  
  | otherwise = if p  mid then binarySearch p (left++[mid])  else binarySearch p (mid:right) where
     nHalf = length  ls `div` 2 -1 
     (left, mid:right) =  splitAt nHalf rest 

solution2 :: [GridPos] -> String
solution2  coordsList =  let (y,x) = coordsList !! n in show x<>","<> show y where   
  n = fromJust .  binarySearch (not . isSolvable) $ [0..length coordsList-1]
  isSolvable n = case solveForN n coordsList of 
    Dist _ -> True 
    _ -> False 

-- >>> solution2 . parseFile <$> readFile "inputs/18.txt"
-- <stderr>: hPutChar: invalid argument (cannot encode character '\8216')

getDists :: CharGrid -> DistanceMap GridPos
getDists  charGrid  =  distMap  where 
  distMap :: A.Array GridPos Distance 
  distMap = distanceMap $   runDijkstraST graph (0,0) [dims]
  graph = makeGraph charGrid 


getSolutions18 :: String -> IO (Int, String)
getSolutions18 = readFile >=> (parseFile >>> (solution1 &&&  solution2) >>> return)

saveGraphPaths :: String -> Int -> String -> IO () 
saveGraphPaths outputFile n inputFile  = do 
  coordList <- parseFile <$> readFile inputFile
  let paths = bestPathsForN n coordList 
      grid = makeGrid n coordList 
      filledGrid = grid // ((coordList !! n, 'N'):[(pos, toEnum (fromEnum '0' + id)) | (path, id) <- zip paths [1..], pos <- path ] )
  saveGridToFile outputFile filledGrid
  
bestPathsForN :: Int -> [GridPos] -> [Path]
bestPathsForN n coordsList = let 
  graph = makeGraph . makeGrid n $  coordsList 
  in bestPaths graph (0,0) dims
