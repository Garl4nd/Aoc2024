{-# LANGUAGE NamedFieldPuns #-}

module N6 (getSolutions6) where

import Control.Arrow
import Control.Monad (forM, (>=>))
import Control.Monad.ST (ST, runST)
import Data.Array.Base (STUArray, freezeSTUArray, modifyArray, readArray, thawSTUArray, writeArray)
import Data.Array.Unboxed ((!), (//))
import qualified Data.Array.Unboxed as A
import Data.List (find, nub, unfoldr)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Useful (CharGridU, countIf, strToCharGrid) -- type CharGridU = A.UArray (Int, Int) Char

type Position = (Int, Int)
data Direction = U | D | L | R deriving (Show, Eq, Ord)
data State = State {pos :: Position, dir :: Direction} deriving (Show, Eq, Ord)

movePos :: Position -> Direction -> Position
movePos (y, x) dir = case dir of
  U -> (y - 1, x)
  D -> (y + 1, x)
  L -> (y, x - 1)
  R -> (y, x + 1)

rotate :: Direction -> Direction
rotate U = R
rotate R = D
rotate D = L
rotate L = U

findPath :: Bool -> State -> CharGridU -> [State]
findPath onlyObstacles initState charGrid = takeWhile (inBounds . pos) $ iterate updateState initState
 where
  updateState state@State{pos, dir}
    | inBounds newPos && charGrid ! newPos == '#' = state{dir = rotate dir}
    | onlyObstacles && inBounds pos = updateState state{pos = newPos}
    | otherwise = state{pos = newPos}
   where
    newPos = movePos pos dir
  inBounds = A.inRange bounds
  bounds = A.bounds charGrid

pathIsLoop :: [State] -> Bool
pathIsLoop = go S.empty
 where
  go :: S.Set State -> [State] -> Bool
  go _ [] = False
  go visitedStates (s : restOfPath)
    | s `S.member` visitedStates = True
    | otherwise = go (S.insert s visitedStates) restOfPath

dirList :: [Char]
dirList = ['^', 'v', '<', '>']

getInitialState :: CharGridU -> State
getInitialState charGrid =
  let
    initField = fromJust $ find (\(_, c) -> c `elem` dirList) $ A.assocs charGrid
    (pos, c) = initField
    charToDir :: Char -> Direction
    charToDir '^' = U
    charToDir 'v' = D
    charToDir '<' = L
    charToDir '>' = R
   in
    State{pos, dir = charToDir c}

insertObstacle :: CharGridU -> Position -> CharGridU
insertObstacle charGrid pos = if charGrid ! pos `elem` '#' : dirList then charGrid else charGrid // [(pos, '#')]

parseFile :: String -> (CharGridU, State)
parseFile file = let charGrid = strToCharGrid file in (charGrid, getInitialState charGrid)

solution1 :: (CharGridU, State) -> Int
solution1 (charGrid, initState) = length . nub $ pos <$> findPath False initState charGrid

solution2 :: (CharGridU, State) -> Int
solution2 (charGrid, initState) = countIf pathIsLoop $ findPath True initState <$> modifiedGrids
 where
  modifiedGrids = insertObstacle charGrid <$> A.indices charGrid

getSolutions6 :: String -> IO (Int, Int)
getSolutions6 = readFile >=> (parseFile >>> (solution1 &&& solution2) >>> return)

solution2' :: (CharGridU, State) -> Int
solution2' (charGrid, initState) = runST $ countLoopsST (thawSTUArray charGrid) -- countIf pathIsLoop $ findPath initState <$> modifiedGrids
 where
  countLoopsST :: ST s (STUArray s Position Char) -> ST s Int
  countLoopsST stAr = do
    ar <- stAr
    paths <- forM [pos | pos <- A.indices charGrid, charGrid ! pos `notElem` '#' : dirList] $ findPathST ar
    return $ countIf pathIsLoop paths
   where
    findPathST ar obstaclePos = do
      writeArray ar obstaclePos '#'
      uAr <- freezeSTUArray ar
      let path = findPath True initState uAr
      writeArray ar obstaclePos '.'
      return path
