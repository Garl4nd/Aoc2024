module N25 (getSolutions25) where

import Control.Arrow
import Control.Monad ((>=>))
import Data.List (partition, transpose)

type ListGrid = [[Char]]
type CondensedGrid = [Int]
data GridType = Key | Lock

parseFile :: String -> ([CondensedGrid], [CondensedGrid])
parseFile file = (map (condense Lock) *** map (condense Key)) . partition isLock . go $ lines file
 where
  go :: [String] -> [ListGrid]
  go [] = []
  go lns =
    let (currentGrid, rest) = splitAt 8 lns
     in take 7 currentGrid : go rest
  isLock = all (== '#') . head
  condense :: GridType -> ListGrid -> [Int]
  condense gridType grid = [length . takeWhile (== '#') $ col | col <- transpose grid']
   where
    grid' = case gridType of
      Lock -> grid
      Key -> reverse grid

fits :: CondensedGrid -> CondensedGrid -> Bool
fits lock key = and $ zipWith (\l k -> l + k <= 7) lock key

solution1 :: ([CondensedGrid], [CondensedGrid]) -> Int
solution1 (locks, keys) = length [() | lock <- locks, key <- keys, fits lock key]

solution2 = const $ 2024 * 50

getSolutions25 :: String -> IO (Int, Int)
getSolutions25 = readFile >=> (parseFile >>> (solution1 &&& solution2) >>> return)
