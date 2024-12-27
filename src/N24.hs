{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module N24 () where 
import qualified Data.Map as M
import Control.Arrow
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Control.Monad (void)
import Text.Megaparsec.Debug (MonadParsecDbg(dbg))
import Data.Either (fromRight)
import Data.Bits 
import GraphMaker (writeUndirectedGraph, writeLabeledGraph)
import Data.List (sort)
import Debug.Trace (trace)
import Debugging (traceWInfo)
import Data.Function.Memoize (memoFix)
type SParser = Parsec Void String

data Node = Node {val :: Maybe Int, op :: OP, edgesIn :: [String], edgesOut :: [String] } deriving Show
data OP = AND | OR | XOR | COMBINE | CONST deriving (Show, Eq, Read) 
type WireGraph = M.Map String Node 

parseFile file = fromRight M.empty $ runParser fileParser "" file
fileParser ::  SParser WireGraph -- [(String, Node)]
fileParser  = let 
  cstNodeParser :: SParser (String, Node)
  cstNodeParser = do 
    (target, value) <- (,) <$> (manyTill anySingle $ string ": ") <*> ( L.decimal)  
    return (target, Node {val = Just value, op = CONST, edgesIn = [], edgesOut = []})
  toBool :: Int -> Bool
  toBool 0 = False 
  toBool _ = True
  opNodeParser :: SParser (String, Node)
  opNodeParser = do 
    n1 <- manyTill anySingle $  char ' '
    opName <- choice $ try . string <$> ["AND", "OR", "XOR"]
    n2 <- char ' ' *> manyTill anySingle (string " -> ")
    target <- manyTill anySingle newline 
    return (target, Node {val = Nothing, op = read opName, edgesIn = [n1, n2], edgesOut = []})
  in do
    startNodes <- manyTill (cstNodeParser <* newline) newline --do
    -- return startNodes
    regularNodes <- manyTill opNodeParser eof 
    let finalNode = ("final", Node {val = Nothing, op = COMBINE, edgesOut = [], edgesIn = sort [name | (name, _) <- startNodes ++ regularNodes, head name == 'z' ] })
    return $ fillOutEdges . M.fromList $ startNodes ++ regularNodes ++ [finalNode] 

fillOutEdges :: WireGraph -> WireGraph 
fillOutEdges  wireGraph = M.mapWithKey fillNode wireGraph where 
  fillNode key node@Node{edgesOut} = let 
      outNodes =  M.keys $  M.filter ((key `elem`). edgesIn) wireGraph 
      in node{edgesOut = edgesOut ++ outNodes}

projectWireGraph :: WireGraph -> M.Map String [(String, Maybe Int)]
projectWireGraph = M.map (\node -> fmap (,val node) . edgesOut $ node)

writeGraph :: String -> String -> IO ()
writeGraph inputFile outputFile = do 
   file <- readFile inputFile 
   writeLabeledGraph outputFile $ projectWireGraph . parseFile $ file

opFunc :: OP -> ([Int] -> Int)
opFunc AND = foldr1 (.&.)
opFunc OR = foldr1 (.|.)
opFunc XOR = foldr1 xor 
opFunc COMBINE = foldr1 (\b num -> shiftL num 1 .|. b ) 
opFunc CONST  = const 0

type Memo f = f-> f
solveGraph :: String -> WireGraph -> Int 
solveGraph target wireGraph  = memoFix go target  where 
  go :: Memo (String -> Int) 
  go go key = case wireGraph M.! key of 
    Node{val = Just value} -> value 
    Node{val = Nothing, op, edgesIn} -> opFunc op $ go <$>  edgesIn   

strRepr :: String-> Int -> String 
strRepr base n = base <> (if n<10 then "0" <> show n else show n ) 

setInputBit ::  String -> Int -> Int -> WireGraph -> WireGraph 
setInputBit  inputType pos value graph= M.mapWithKey (\key node -> 
 if take 1 key == inputType then 
                              node{val = Just (if key == strRepr inputType pos then value else 0)} else node) graph  

setNumber ::  String -> Int -> WireGraph -> WireGraph 
setNumber  inputType value graph =foldr (\(pos, bitVal) accMap -> M.adjust 
   (\node -> node{val= Just bitVal}) (strRepr inputType pos)  accMap) graph $ zip [0..] $ numBinary value 44 


checkNthBit :: WireGraph -> Int -> Int -> Int -> (Int, Int, Bool) 
checkNthBit wireGraph pos xval yval = let newGraph = setInputBit "x" pos xval $ setInputBit "y" pos yval wireGraph 
                                          result = solveGraph  "final" newGraph
                                          sum = shiftL xval pos + shiftL yval pos 
                                          in (result, sum, result == sum)


numBinary n places =  map (\place -> shiftR n place .&. 1) [0..places-1] 
checkNumbers :: WireGraph -> Int -> Int -> (Int, Int, Bool)
checkNumbers graph x y = let newGraph = setNumber "x" x $ setNumber "y" y graph 
                             result = solveGraph "final" newGraph 
                             sum = x+y 
                             in (result, sum, result == sum)
passGates :: Int -> WireGraph -> [String] 
passGates n wireGraph = memoFix go $ strRepr "z" n where 
  go :: Memo (String -> [String]) 
  go go key = case wireGraph M.! key of 
    Node{val = Just value} -> [key] 
    Node{val = Nothing, edgesIn} -> key: concatMap go edgesIn   

