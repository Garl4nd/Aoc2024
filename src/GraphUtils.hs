{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Rank2Types #-}

module GraphUtils (runDijkstraST, addDist, distanceToInt, distanceMap, bestPaths, LabeledGraph, ArrayGraph, Distance (Dist, Inf), Num, DijkstraState, DistanceMap) where 

import qualified Data.Array as A 
import Data.Array ((!))
import qualified Data.Heap as H
import qualified Data.Set as S
import GHC.List (foldl')
import Control.Monad.ST 
import Data.Array.ST (runSTArray, newArray, writeArray, readArray, STArray)--, unsafeThaw)
import Control.Monad (forM_, forM)

type NumType = Int
type Edges node  = [(node, NumType)]
type DistanceMap node = A.Array node Distance 
type DistanceMapST node s = STArray s node Distance 
type ArrayGraph node = A.Array node (Edges node)
data Distance =  Dist NumType | Inf deriving (Eq, Show)
--type Distance = NumType
distanceToInt :: Distance -> Int 
distanceToInt Inf = 1000000000000000
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

type Path node = [node]

bestPaths :: forall graph node. (Show node, LabeledGraph graph node) => graph ->  node -> node -> [Path node]
bestPaths graph start end = if distMap ! end == Inf then [] else go end where  
    go :: node -> [Path node]
    go pos
            | pos == start = [[pos]]
            | otherwise = let 
                neighbors =  getEdges reversedGraph pos
                departureNodes = [node | (node, val) <- neighbors, addDist (distMap ! node) val == distMap ! pos ]
                in [pos:  path |  path <- concatMap go  departureNodes  ]
    reversedGraph = graph -- actually reverse for directed graphs
    distMap = distanceMap $ runDijkstraST graph start [end] -- getCompleteDistMap ar         