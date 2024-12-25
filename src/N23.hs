module N23 () where 
import qualified Data.Map as M
import GraphUtils
import Useful (wordsWhen)
import qualified Data.Set as S 
import GraphMaker
import Data.List (sortOn, intercalate)
type StringGraph = M.Map String [String]
type Clique = S.Set String
parseFile :: String -> StringGraph 
parseFile file = foldr updateGraph M.empty $ lines file where 
  updateGraph line graph = case wordsWhen (=='-') line of
    [] -> graph 
    [leftPc, rightPc] -> add leftPc rightPc $ add rightPc leftPc graph
  add node edge = M.insertWith (++) node [edge] 

cliqueRequirement :: Clique -> Bool 
cliqueRequirement clique = S.size clique == 3 && any (('t'==).head) clique 

cliques :: StringGraph -> S.Set Clique
cliques = bronKerbosch 

binom :: Int -> Int -> Int 
binom n 0 = 1
binom n k 
  | k> n = 0 
  | otherwise =  binom (n-1) k + binom (n-1) (k-1)

countNCliques :: Int -> Clique -> Int 
countNCliques n clique = let 
    tNum = S.size $ S.filter (('t'==).head) clique 
    cSize = S.size clique 
    in if  cSize <n || tNum == 0  then 0 else binom cSize n - binom (cSize-tNum) n  

solution1 :: StringGraph -> Int 
solution1 graph = let 
  cliques = S.unions $ S.map (S.filter cliqueRequirement . S.powerSet) $ bronKerbosch graph 
  in S.size cliques

solution2 :: StringGraph -> String 
solution2 graph = let 
  cliques = sortOn (negate . S.size) . S.toList  $ bronKerbosch  graph 
  in intercalate ","  . S.toList $ head cliques

