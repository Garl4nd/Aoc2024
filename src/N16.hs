{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Rank2Types #-}

module N16
    (getSolutions16) where
import Control.Arrow
import Control.Monad ((>=>))
import  qualified Data.Array.Unboxed as A
import Data.Array.Unboxed ((!))
import qualified Data.Heap as H
import qualified Data.Set as S
import Useful (strToCharGrid, CharGrid, GridPos )
import GHC.List (foldl')
import Control.Monad.ST 
import Data.Array.ST (runSTArray, newArray, writeArray, readArray, STArray)--, unsafeThaw)
import Control.Monad (forM_, forM)
import Data.List (nub)


data Orientation = H | V deriving (Show, Eq, Ord, A.Ix)

type AugPos = (GridPos, Orientation) 
type NumType = Int
type Edges node  = [(node, NumType)]
type ArrayGraph node = A.Array node (Edges node)
type DistanceMap node = A.Array node Distance 
type DistanceMapST node s = STArray s node Distance 

data Distance =  Dist NumType | Inf deriving (Eq, Show)
--type Distance = NumType
distanceToInt :: Distance -> Int 
distanceToInt Inf = 1000000000
distanceToInt (Dist n) = n 

class A.Ix node =>  LabeledGraph graph node  where
    getEdges ::graph -> node -> [(node, NumType)]
    getBounds ::  graph ->  (node, node)

instance A.Ix node => LabeledGraph (A.Array node (Edges node)) node  where
    getEdges :: A.Array node (Edges node) -> node -> [(node, NumType)]
    getEdges  = (!) --graphAr ! node
    getBounds = A.bounds 
    
instance Ord Distance where    
    (<=) :: Distance -> Distance -> Bool
    Inf <= Inf = True
    Inf <= Dist _ = False
    Dist _ <= Inf = True
    Dist x <= Dist y = x <=y

addDist :: Distance -> NumType -> Distance
addDist (Dist x) y = Dist (x+y)
addDist _ _ = Inf

data DijkstraState node = DijkstraState    {
    finalStates :: S.Set node, 
    distanceMap :: DistanceMap node,
    nodeQueue :: H.MinPrioHeap Distance node,
    dests :: S.Set node
} deriving Show

data DijkstraStateST node s = DijkstraStateST    {
    finalStatesST :: S.Set node, 
    distanceMapST :: DistanceMapST node s,
    nodeQueueST :: H.MinPrioHeap Distance node,
    destsST :: S.Set node
} 

dijkstraLoop :: forall node graph s. (Show node, LabeledGraph graph node) => graph -> ST s (DijkstraStateST node s) ->  ST s (DijkstraStateST node s)
dijkstraLoop graph dsST = do
    ds@DijkstraStateST {finalStatesST = fs, distanceMapST = dm, nodeQueueST = nq, destsST = dsts}  <- dsST    
    if S.null dsts  then 
        dsST -- return ds
    else
        case H.view nq of 
            Nothing ->  dsST -- return ds
            Just (distNode, nq')  ->   processNode distNode where
                processNode :: (Distance, node) ->  ST s (DijkstraStateST node s)
                processNode (_, minNode) 
                    |  S.member minNode fs =  dijkstraLoop graph (return $ ds {nodeQueueST = nq'}  )                                 
                processNode (dist, minNode) = do                                                             
                    let candidateList = [ (neighbor, newDist) | (neighbor, edgeVal) <- getEdges graph  minNode, S.notMember neighbor fs,  let newDist = addDist dist  edgeVal]
                    currentDists <- forM candidateList (\(neigh, _) -> readArray dm neigh)
                    let updateList = [ (neighbor, newDist) | ((neighbor, newDist), currentDist) <- zip candidateList currentDists, newDist <= currentDist  ] 
                        updatedQueue = foldl' (\q (node, dist')    ->  H.insert (dist', node) q  ) nq' updateList             
                    forM_ updateList $ \(pos, newDist) -> writeArray dm pos newDist                    
                    dijkstraLoop graph  (return $ ds {finalStatesST = S.insert  minNode fs, distanceMapST = dm, nodeQueueST = updatedQueue, destsST = S.delete minNode dsts } )



runDijkstraST ::  forall graph node. (Show node, LabeledGraph graph node) => graph -> node -> [node]  -> DijkstraState node
runDijkstraST graph start ends =  extractFromST $ dijkstraLoop graph initState  where
    extractFromST :: (forall s. ST s (DijkstraStateST node s)) -> DijkstraState node
    extractFromST stRes = DijkstraState {finalStates = fs, distanceMap = dm, nodeQueue = nq, dests = dst } where
        dm = runSTArray $ do  
            DijkstraStateST { distanceMapST = dmST } <- stRes -- runDijkstraST graph start ends
            return dmST
        (fs, nq, dst )= runST  $ do
            DijkstraStateST { destsST = dst, nodeQueueST = nq', finalStatesST = fs' }  <- stRes
            return (fs',  nq',dst)            
    initState ::  ST s (DijkstraStateST node s)
    initState =  do 
                            stAr <- newArray (getBounds graph ) Inf
                            writeArray stAr start  $ Dist 0
                            return DijkstraStateST {finalStatesST = S.empty, distanceMapST = stAr, nodeQueueST = H.singleton ( Dist 0, start), destsST = S.fromList ends } -- runDijkstraSTLow graph start ends

solveWDijkstraST :: (Show node, LabeledGraph graph node) => graph -> node  -> [node] -> DistanceMap node
solveWDijkstraST   = ((distanceMap.).) . runDijkstraST 

solveAugmentedGraph :: ArrayGraph AugPos -> GridPos -> GridPos ->  Distance
solveAugmentedGraph graph start end = let initStates = [(start, ori) | ori <- [H] ]
                                          endStates =  [(end, ori) | ori <- [H, V]]                                  
                                          in minimum $ concat [
                                          let distMap =  solveWDijkstraST graph initState endStates in map (distMap !) endStates | initState <- initStates]

genAugEdges :: AugPos ->  (GridPos, GridPos) -> CharGrid ->  Edges AugPos
genAugEdges augpos@(pos, ori) ((ymin, xmin), (ymax, xmax)) nodeAr  = if nodeAr ! pos == '#' then [] else edges where          
     neighbors ((y,x), H) = [((y,x+1),H),((y,x-1),H), ((y,x),V)]
     neighbors ((y,x), V) = [((y-1,x), V),((y+1,x),V), ((y,x),H)]
     edges = [(neighbor, if neiDir == ori then 1 else 1000)| neighbor@(neiPos, neiDir) <- neighbors augpos, inBounds neiPos, nodeAr ! neiPos /= '#'     ]
     inBounds (y,x) = ymin <= y && y <= ymax && xmin <= x && x <= xmax                                        

nodeMapToAugmentedGraph :: CharGrid ->  ArrayGraph AugPos
nodeMapToAugmentedGraph nodeMap  = A.listArray augBounds listVals where
    bounds@((ymin, xmin), (ymax, xmax)) = A.bounds nodeMap
    augBounds = (((ymin, xmin), H), ((ymax, xmax), V))    
    augCoords = [((y,x), ori) | y <- [ymin..ymax], x <- [xmin..xmax], ori <- [H,V]]
    listVals = [  genAugEdges augCoord bounds nodeMap | augCoord <- augCoords]

type Path = [GridPos]


bestPaths :: CharGrid ->  AugPos -> GridPos -> [Path]
bestPaths grid start endPos = go augEnd where  
    go :: AugPos -> [Path]
    go augPos@(pos, _)
            | augPos == start = [[pos]]
            | otherwise = let 
                neighbors =  reversedGraph !  augPos
                departureNodes = [node | (node, val) <- neighbors, addDist (distMap ! node) val == distMap ! augPos ]
                in [pos:  path |  path <- concatMap go  departureNodes  ]
    graph = nodeMapToAugmentedGraph grid
    reversedGraph = graph -- actually reverse for directed graphs
    distMap = distanceMap $ runDijkstraST graph start [(endPos,H), (endPos, V)] -- getCompleteDistMap ar     
    augEnd = let bestDir = if distMap ! (endPos,H) < distMap ! (endPos,V) then H else V in (endPos, bestDir)
    
            
solution1:: CharGrid -> Int
solution1 grid = let graph =  nodeMapToAugmentedGraph grid -- <$> readFile "inputs\\16_test.txt"
                     (((yMin, xMin),_), ((yMax, xMax),_)) = A.bounds graph 
                     start = (yMax-1, xMin+1)
                     end = (yMin+1, xMax -1)
                 in  distanceToInt $ solveAugmentedGraph  graph start end

solution2:: CharGrid -> Int
solution2 grid =  length .nub  . concat $ bestPaths grid (start,H) end where    
    start = (yMax-1, xMin+1)
    end = (yMin+1, xMax -1) 
    ((yMin, xMin), (yMax, xMax)) = A.bounds grid

getSolutions16 :: String -> IO (Int, Int)
getSolutions16 = readFile >=> (strToCharGrid >>> (solution1 &&& solution2) >>> return)
