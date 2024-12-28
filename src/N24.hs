{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module N24 (getSolutions24) where

import Control.Arrow
import Control.Monad (void)
import Data.Bits
import Data.Either (fromRight)
import Data.Function.Memoize (memoFix)
import Data.Int (Int64)
import Data.List (intercalate, partition, sort, sortOn)
import qualified Data.List as S
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Void (Void)
import Debug.Trace (trace)
import Debugging (traceWInfo)
import GraphMaker (writeLabeledGraph, writeUndirectedGraph)
import System.Random
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (MonadParsecDbg (dbg))
import Useful (pairChoices)

type SParser = Parsec Void String

data Node = Node {val :: Maybe Int64, op :: OP, edgesIn :: [String], edgesOut :: [String]} deriving (Show)
data OP = AND | OR | XOR | COMBINE | CONST deriving (Show, Eq, Read)
type WireGraph = M.Map String Node

parseFile file = fromRight M.empty $ runParser fileParser "" file
fileParser :: SParser WireGraph -- [(String, Node)]
fileParser =
  let
    cstNodeParser :: SParser (String, Node)
    cstNodeParser = do
      (target, value) <- (,) <$> (manyTill anySingle $ string ": ") <*> (L.decimal)
      return (target, Node{val = Just value, op = CONST, edgesIn = [], edgesOut = []})
    toBool :: Int64 -> Bool
    toBool 0 = False
    toBool _ = True
    opNodeParser :: SParser (String, Node)
    opNodeParser = do
      n1 <- manyTill anySingle $ char ' '
      opName <- choice $ try . string <$> ["AND", "OR", "XOR"]
      n2 <- char ' ' *> manyTill anySingle (string " -> ")
      target <- manyTill anySingle newline
      return (target, Node{val = Nothing, op = read opName, edgesIn = [n1, n2], edgesOut = []})
   in
    do
      startNodes <- manyTill (cstNodeParser <* newline) newline -- do
      -- return startNodes
      regularNodes <- manyTill opNodeParser eof
      let finalNode = ("final", Node{val = Nothing, op = COMBINE, edgesOut = [], edgesIn = sort [name | (name, _) <- startNodes ++ regularNodes, head name == 'z']})
      return $ fillOutEdges . M.fromList $ startNodes ++ regularNodes ++ [finalNode]

fillOutEdges :: WireGraph -> WireGraph
fillOutEdges wireGraph = M.mapWithKey fillNode wireGraph
 where
  fillNode key node@Node{edgesOut} =
    let
      outNodes = M.keys $ M.filter ((key `elem`) . edgesIn) wireGraph
     in
      node{edgesOut = outNodes}

projectWireGraph :: WireGraph -> M.Map String [(String, String)]
projectWireGraph = M.map (\node -> fmap (,(show $ val node) <> " " <> (show $ op node)) . edgesOut $ node) . fillOutEdges

writeGraph :: String -> String -> IO ()
writeGraph inputFile outputFile = do
  file <- readFile inputFile
  writeLabeledGraph outputFile $ projectWireGraph . parseFile $ file

opFunc :: OP -> ([Int64] -> Int64)
opFunc AND = foldr1 (.&.)
opFunc OR = foldr1 (.|.)
opFunc XOR = foldr1 xor
opFunc COMBINE = foldr1 (\b num -> shiftL num 1 .|. b)
opFunc CONST = const 0

type Memo f = f -> f
solveGraph :: String -> WireGraph -> Int64
solveGraph target wireGraph = memoFix go target
 where
  go :: Memo (String -> Int64)
  go go key = case wireGraph M.! key of
    Node{val = Just value} -> value
    Node{val = Nothing, op, edgesIn} -> opFunc op $ go <$> edgesIn

strRepr :: String -> Int -> String
strRepr base n = base <> (if n < 10 then "0" <> show n else show n)

setNumber :: String -> Int64 -> WireGraph -> WireGraph
setNumber inputType value graph =
  foldr
    ( \(pos, bitVal) ->
        M.adjust
          (\node -> node{val = Just bitVal})
          (strRepr inputType pos)
    )
    graph
    $ zip [0 ..]
    $ numBinary value

maxPlaces :: Int
maxPlaces = 45
numBinary :: Int64 -> [Int64]
numBinary n = map (\place -> shiftR n place .&. 1) [0 .. maxPlaces - 1]

checkNumbers :: WireGraph -> Int64 -> Int64 -> (Int64, Int64, Bool)
checkNumbers graph x y =
  let newGraph = setNumber "x" x $ setNumber "y" y graph
      result = solveGraph "final" newGraph
      addition = x + y
   in (result, addition, result == addition)

testRandomRange :: WireGraph -> Int -> (Int64, Int64) -> (Int, Int)
testRandomRange graph reps (minNum, maxNum) =
  let
    genX = mkStdGen 15789
    genY = mkStdGen 45687
    xs = take reps $ randomRs (minNum, maxNum) genX
    ys = take reps $ randomRs (minNum, maxNum) genY
   in
    let (correct, wrong) = partition (\(_, _, same) -> same) $ zipWith (checkNumbers graph) xs ys
     in (length correct, length wrong)

intersectList :: (Ord k) => [S.Set k] -> [S.Set k]
intersectList = scanl1 S.intersection

swapCandidates :: WireGraph -> Int -> [S.Set (String, String)]
swapCandidates graph n = intersectList $ [S.fromList $ goodSwaps graph passGateMap (n - 1) x y | let pow = 2 ^ n, x <- [0, pow, 2 * pow], y <- [0, pow, 2 * pow], let (_, _, same) = checkNumbers graph x y, not same]
 where
  passGateMap = M.fromList [(key, passGates graph key) | key <- M.keys graph]

swapList = ["rst", "z07", "jpj", "z12", "kgj", "z26", "chv", "vvw"]

rectifiedGraphs :: WireGraph -> [WireGraph]
rectifiedGraphs graph =
  let
    graph' = swappedGraph graph "rts" "z07"
    graph'' = swappedGraph graph' "jpj" "z12"
    graph''' = swappedGraph graph'' "kgj" "z26"
    graph4 = swappedGraph graph''' "chv" "vvw"
   in
    [graph, graph', graph'', graph''', graph4]

generateSwapCandidates :: WireGraph -> [([(String, String)], WireGraph)]
generateSwapCandidates graph =
  let
    wrongLevels = fromIntegral <$> [n | n <- [0 .. 44], let (_, _, same) = checkNumbers graph (2 ^ n) 1, not same]
    swapGraphs graph' n = [(from, to, swappedGraph graph' from to) | (from, to) <- let cands = traceWInfo False "cands" (swapCandidates graph' n) in if null cands then [] else S.toList $ last cands]
    candidateList =
      foldl
        ( \candidates (n, m) ->
            concat
              [ [((from, to) : swapHistory, graph'') | (from, to, graph'') <- swapGraphs graph' (traceWInfo False "n" n)]
              | (swapHistory, graph') <- candidates
              ]
        )
        [([], graph)]
        $ zip (traceWInfo True "wls" wrongLevels) (tail wrongLevels ++ [43])
   in
    candidateList -- foldl (\(n, m )

goodSwaps :: WireGraph -> M.Map String [String] -> Int -> Int64 -> Int64 -> [(String, String)]
goodSwaps graph gateMap goodZ x y =
  let
    goodKeys = gateMap M.! strRepr "z" goodZ
    suspicousKeys = [key | key <- gateMap M.! strRepr "z" (goodZ + 1) ++ gateMap M.! strRepr "z" (goodZ + 2), key `notElem` goodKeys, notStart key] -- fst <$> M.toList (countSingle graph x y)
    notStart = (`notElem` ['x', 'y']) . head
    pairs = pairChoices suspicousKeys
   in
    [(from, to) | (from, to) <- pairs, from `notElem` passGates graph to, to `notElem` passGates graph from, let (_, _, same) = checkNumbers (swappedGraph graph from to) x y, same]

swappedGraph :: WireGraph -> String -> String -> WireGraph
swappedGraph graph from to =
  let changeInEdges from to = M.insert from $ let node = graph M.! to in node -- {edgesIn = edgesIn $ graph M.! from}
   in changeInEdges to from $ changeInEdges from to graph

passGates :: WireGraph -> String -> [String]
passGates wireGraph = memoFix go
 where
  go :: Memo (String -> [String])
  go go key = case wireGraph M.! key of
    Node{val = Just _} -> [key]
    Node{val = Nothing, edgesIn} -> key : concatMap go edgesIn

solution1 :: WireGraph -> Int
solution1 = fromIntegral . solveGraph "final"
solution2 :: WireGraph -> [(String, String)]
solution2 graph =
  let candidates = generateSwapCandidates graph
      tests (_, graph') = testRandomRange graph' 100 (0, 2 ^ 44)
      perfectCandidates = head $ filter (\candidate -> let (_, wrong) = tests candidate in wrong == 0) candidates
   in fst perfectCandidates

getSolutions24 :: String -> IO (Int, Int)
getSolutions24 filename = do
  file <- readFile filename
  let graph = parseFile file
  print $ solution2 graph
  -- print $ intercalate "\n" . map show $ generateSwapCandidates graph
  return (solution1 graph, 0)
