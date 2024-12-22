module N21 (getSolutions21) where

import Control.Arrow
import Control.Monad ((>=>))
import Data.Array ((!))
import qualified Data.Array as A
import Data.Function.Memoize (memoFix2)
import qualified Data.Map as M
import GraphUtils
import Useful

makeGraph :: CharGrid -> ArrayGraph GridPos
makeGraph grid = A.array bounds edgeAssocs
 where
  bounds = A.bounds grid
  positions = A.indices grid
  edgeAssocs = makeEdges <$> positions
  makeEdges pos = (pos, [(nei, 1) | nei <- neighbors4 pos, valid nei])
  valid pos' = A.inRange bounds pos' && grid ! pos' /= hole

u, d, l, r, enter, hole :: Char
u = '^'
d = 'v'
l = '<'
r = '>'
enter = 'A'
hole = 'h'

numGrid :: CharGrid
numGrid = A.listArray ((1, 1), (4, 3)) $ [hole, '0', enter] ++ ['1' .. '9']

dirGrid :: CharGrid
dirGrid = A.listArray ((1, 1), (2, 3)) [l, d, r, hole, u, enter]

dirPaths :: CharGrid -> KeyMap
dirPaths grid =
  let
    graph = makeGraph grid
    posAssocs = [((src, tg), bestPaths graph src tg) | src <- A.indices grid, tg <- A.indices grid, grid ! src /= hole, grid ! tg /= hole]
    valAssocs = [((grid ! src, grid ! tg), [path ++ [enter] | path <- gridposPathToDirs <$> paths]) | ((src, tg), paths) <- posAssocs]
   in
    M.fromList valAssocs

gridposPathToDirs :: [GridPos] -> [Char]
gridposPathToDirs ls = concat $ zipWith (\a b -> [posToDir a b | a /= b]) ls (tail ls)

posToDir :: GridPos -> GridPos -> Char
posToDir (ys, xs) (yt, xt)
  | yt == ys && xt == xs - 1 = l
  | yt == ys && xt == xs + 1 = r
  | yt == ys - 1 && xt == xs = d
  | yt == ys + 1 && xt == xs = u
  | otherwise = error "wrong position"

type KeyMap = M.Map (Char, Char) [(Path Char)]
numKeyMap = dirPaths numGrid
dirKeyMap = dirPaths dirGrid

elementaryPaths :: KeyMap -> Char -> Char -> [Path Char]
elementaryPaths keymap src tg = keymap M.! (src, tg) -- [path ++ [a] | path <- keymap M.! (src, tg)]

type Memo f = f -> f

remotePressCount :: [KeyMap] -> [Char] -> Int
remotePressCount keymaps kseq = sum $ map (goM (length keymaps)) $ startEndPairs kseq
 where
  startEndPairs path = zip (enter : path) path
  goM = memoFix2 go
  go :: Memo (Int -> (Char, Char) -> Int)
  go _ 0 _ = 1
  go go n (start, end) =
    let
      keymap = keymaps !! (n - 1)
      candidatePaths = elementaryPaths keymap start end
      subLengths = [go (n - 1) <$> startEndPairs path | path <- candidatePaths]
     in
      minimum $ sum <$> subLengths

complexity :: Int -> [Char] -> Int
complexity n kseq =
  let seqLen = remotePressCount (replicate n dirKeyMap ++ [numKeyMap]) kseq -- pressRemoteSequence' (replicate 3 dirKeyMap ++ [numKeyMap]) kseq
      numPart = read . take 3 $ kseq
   in seqLen * numPart

solution1 :: [String] -> Int
solution1 = sum . map (complexity 2)

solution2 :: [String] -> Int
solution2 = sum . map (complexity 25)

getSolutions21 :: String -> IO (Int, Int)
getSolutions21 = readFile >=> (lines >>> (solution1 &&& solution2) >>> return)
