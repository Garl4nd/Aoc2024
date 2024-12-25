module N23 () where 
import qualified Data.Map as M
import GraphUtils
import Useful (wordsWhen)
import qualified Data.Set as S 
import GraphMaker
type StringGraph = M.Map String [String]
type Clique = S.Set String
parseFile :: String -> StringGraph 
parseFile file = foldr updateGraph M.empty $ lines file where 
  updateGraph line graph = case wordsWhen (=='-') line of
    [] -> graph 
    [leftPc, rightPc] -> add leftPc rightPc $ add rightPc leftPc graph
  add node edge = M.insertWith (++) node [edge] 

cliqueRequirement :: Clique -> Bool 
cliqueRequirement clique = S.size clique >= 3 && any (('t'==).head) clique 

cliques :: StringGraph -> S.Set Clique
cliques = bronKerbosch 

