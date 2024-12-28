module N25 (getSolutions25) where

import Control.Arrow
import Control.Monad ((>=>))
import qualified Data.Array as A
import Data.List (partition, transpose)
import Useful

type ListGrid = [[Char]]
type CondensedGrid = [Int]
parseFile :: String -> ([CondensedGrid], [CondensedGrid])
parseFile file = (map (condense Lock) *** map (condense Key)) . classify . go $ lines file
 where
  go :: [String] -> [ListGrid]
  go [] = []
  go lines =
    let (thisPart, rest) = splitAt 8 lines
     in (take 7 $ thisPart) : go rest

classify :: [ListGrid] -> ([ListGrid], [ListGrid])
classify = partition isLock
 where
  isLock = all (== '#') . head

data GridType = Key | Lock
condense :: GridType -> ListGrid -> [Int]
condense Lock grid = [length . takeWhile (== '#') $ col | col <- transpose grid]
condense Key grid = [length . takeWhile (== '#') $ col | col <- transpose . reverse $ grid]

fits :: CondensedGrid -> CondensedGrid -> Bool
fits lock key = and $ zipWith (\l k -> l + k <= 7) lock key

solution :: ([CondensedGrid], [CondensedGrid]) -> Int
solution (locks, keys) = length [[] | lock <- locks, key <- keys, fits lock key]

getSolutions25 :: String -> IO (Int, Int)
getSolutions25 = readFile >=> (parseFile >>> (solution &&& (const $ 50 * 24)) >>> return)

-- >>> parseFile <$> readFile "inputs/25.txt"
-- getLinkDeps: Home module not loaded Useful main-9d09fbcc05f48f36b790b3f194f6b63b4d88732d
