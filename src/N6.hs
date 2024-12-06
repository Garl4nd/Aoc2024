{-# LANGUAGE NamedFieldPuns #-}

module N6 () where

import Control.Monad.Trans.Reader (Reader)
import Data.Array.Unboxed ((!))
import qualified Data.Array.Unboxed as A
import Data.List (find, nub, unfoldr)
import Data.Maybe (fromJust)
import Debugging (traceWInfo)
import Useful (CharGrid, strToCharGrid)

type Position = (Int, Int)
data Direction = U | D | L | R deriving (Show, Eq)
data State = State {pos :: Position, dir :: Direction} deriving (Show, Eq)

movePos :: Position -> Direction -> Position
movePos (y, x) dir = case dir of
  U -> (y - 1, x)
  D -> (y + 1, x)
  L -> (y, x - 1)
  R -> (y, x + 1)

-- moveState :: State -> State
-- moveState state@State{pos = p0, dir = d}  = state{pos = movePos p0 d}
rotate :: Direction -> Direction
rotate U = R
rotate R = D
rotate D = L
rotate L = U

findPath :: State -> CharGrid -> [Position]
findPath initState charGrid = unfoldr updateState initState
 where
  updateState state@State{pos, dir}
    | outOfBounds pos = Nothing
    | not (outOfBounds newPos) && charGrid ! newPos == '#' = Just (pos, state{dir = rotate dir})
    | otherwise = Just (pos, state{pos = newPos})
   where
    newPos = movePos pos dir
    outOfBounds = not . A.inRange bounds
    bounds = A.bounds charGrid

charToDir :: Char -> Direction
charToDir '^' = U
charToDir 'v' = D
charToDir '<' = L
charToDir '>' = R

getInitialState :: CharGrid -> State
getInitialState charGrid =
  let
    initField = fromJust $ find (\(_, c) -> c `elem` ['^', 'v', '<', '>']) $ A.assocs charGrid
    (pos, c) = initField
   in
    State{pos, dir = charToDir c}

solution1 :: String -> Int
solution1 file = length . nub $ findPath initState charGrid
 where
  initState = traceWInfo True "initState" $ getInitialState charGrid
  charGrid = strToCharGrid file
