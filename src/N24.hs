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
projectWireGraph = M.map (\node -> fmap (,(show $ val node) <> " " <> (show $ op node)) . edgesOut $ node)

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

setInputBit :: String -> Int -> Int64 -> WireGraph -> WireGraph
setInputBit inputType pos value graph =
  M.mapWithKey
    ( \key node ->
        if take 1 key == inputType
          then
            node{val = Just (if key == strRepr inputType pos then value else 0)}
          else node
    )
    graph

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

checkNthBit :: WireGraph -> Int -> Int64 -> Int64 -> (Int64, Int64, Bool)
checkNthBit wireGraph pos xval yval =
  let newGraph = setInputBit "x" pos xval $ setInputBit "y" pos yval wireGraph
      result = solveGraph "final" newGraph
      sum = shiftL xval pos + shiftL yval pos
   in (result, sum, result == sum)

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

type StringCounter = M.Map String Int

updateWrongGateCounter :: WireGraph -> Int64 -> Int64 -> StringCounter -> StringCounter
updateWrongGateCounter graph x y counter =
  let
    (result, addition, same) = checkNumbers graph x y
    resultBits = numBinary result
    addBits = numBinary addition
    difBits = [pos | (rb, ab, pos) <- zip3 resultBits addBits [0 ..], rb /= ab]
    wrongGateCandidates = passGates graph <$> strRepr "z" <$> difBits
    updateCounter :: [String] -> StringCounter -> StringCounter
    updateCounter gates counter = foldr (\key -> M.insertWith (+) key 1) counter gates
    counter' = foldr updateCounter counter wrongGateCandidates
   in
    if same then counter else counter'

countSingle g x y = updateWrongGateCounter g x y M.empty

testManyRandom :: WireGraph -> Int -> (Int64, Int64) -> (Int, Int)
testManyRandom graph reps (minNum, maxNum) =
  let
    genX = mkStdGen 15789
    genY = mkStdGen 45687
    xs = take reps $ randomRs (minNum, maxNum) genX
    ys = take reps $ randomRs (minNum, maxNum) genY
   in
    let (correct, wrong) = partition (\(_, _, same) -> same) $ zipWith (checkNumbers graph) xs ys
     in (length correct, length wrong)

countManyGates :: WireGraph -> Int -> (Int64, Int64) -> StringCounter
countManyGates graph reps (minNum, maxNum) =
  let
    xs = [2 ^ n | n <- [minNum .. maxNum]]
   in
    foldr (uncurry $ updateWrongGateCounter graph) M.empty $ zip xs xs

countInnerGates :: WireGraph -> Int -> (Int64, Int64) -> [(String, Int)]
countInnerGates = (((sortOn (negate . snd) . filter (\(s : _, _) -> s `notElem` ['x', 'y', 'z']) . M.toList) .) .) . countManyGates

intersectList :: (Ord k) => [S.Set k] -> [S.Set k]
intersectList = scanl1 S.intersection

swapCandidates :: WireGraph -> Int -> Int -> [S.Set (String, String)]
swapCandidates graph n m = intersectList $ [S.fromList $ goodSwaps graph (n - 1) x y | x <- 0 : map (2 ^) [n .. m], y <- [2 ^ n .. 2 ^ n + 3], let (_, _, same) = checkNumbers graph x y, not same]

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

generateSwapCandidates :: WireGraph -> [[([(String, String)], WireGraph)]]
generateSwapCandidates graph =
  let
    wrongLevels = fromIntegral <$> [n | n <- [0 .. 44], let (_, _, same) = checkNumbers graph (2 ^ n) 1, not same]
    swapGraphs graph' n m = [(from, to, swappedGraph graph' from to) | (from, to) <- S.toList . last $ swapCandidates graph' n m]
    candidateList =
      -- map (map fst)
      scanl
        ( \candidates (n, m) ->
            concat
              [ [((from, to) : swapHistory, graph'') | (from, to, graph'') <- swapGraphs graph' n (m - 1)]
              | (swapHistory, graph') <- candidates
              ]
        )
        [([], graph)]
        $ zip (traceWInfo True "wls" wrongLevels) (tail wrongLevels ++ [44])
   in
    candidateList -- foldl (\(n, m )

goodSwaps :: WireGraph -> Int -> Int64 -> Int64 -> [(String, String)]
goodSwaps graph goodZ x y =
  let
    goodKeys = passGates graph (strRepr "z" goodZ)
    suspicousKeys = fst <$> M.toList (countSingle graph x y)
    notStart key = head key `notElem` ['x', 'y']
    possibleTargets from = [key | key <- M.keys graph, from `notElem` passGates graph key, notStart key, key `notElem` passGates graph from]
    pairs = S.toList $ S.fromList [if (from < to) then (from, to) else (to, from) | from <- suspicousKeys, notStart from, from `notElem` goodKeys, to <- possibleTargets from, to `notElem` goodKeys, from /= to]
   in
    [(from, to) | (from, to) <- pairs, let (_, _, same) = checkNumbers (swappedGraph graph from to) x y, same]

swappedGraph :: WireGraph -> String -> String -> WireGraph
swappedGraph graph from to =
  let changeInEdges from to = M.insert from $ let node = graph M.! to in node -- {edgesIn = edgesIn $ graph M.! from}
   in fillOutEdges $ changeInEdges to from $ changeInEdges from to graph

--  wrongGates = countInnerGates graph 1 (0, 6)
passGates :: WireGraph -> String -> [String]
passGates wireGraph key = memoFix go $ key
 where
  go :: Memo (String -> [String])
  go go key = case wireGraph M.! key of
    Node{val = Just _} -> [key]
    Node{val = Nothing, edgesIn} -> key : concatMap go edgesIn

solution2 :: WireGraph -> [(String, String)]
solution2 graph =
  let candidates = last $ generateSwapCandidates graph
      tests (_, graph') = testManyRandom graph' 1000 (0, 2 ^ 44)
      perfectCandidates = head $ filter (\candidate -> let (_, wrong) = tests candidate in wrong == 0) candidates
   in fst perfectCandidates

getSolutions24 :: String -> IO (Int, Int)
getSolutions24 filename = do
  file <- readFile filename
  let graph = parseFile file
  print $ solution2 graph
  -- print $ intercalate "\n" . map show $ generateSwapCandidates graph
  return (0, 0)
