{-# LANGUAGE FlexibleContexts #-}

module Useful (
  wordsWhen,
  splitOn,
  readStrList,
  splitBySubstr,
  writeXY,
  consecutivePairs,
  trimSpace,
  trimChar,
  countIf,
  groupBySorted,
  groupByUnique,
  pairChoices,
  pairVariations,
  strToCharGrid,
  charGridToStr,
  CharGrid,
  CharGridU,
  GridPos,
) where

import qualified Data.Array.Unboxed as A
import Data.Function (on)
import Data.List (findIndex, groupBy, inits, intercalate, isPrefixOf, sortOn, tails)
import Data.Tuple (swap)

type GridPos = (Int, Int)
type CharGridU = A.UArray GridPos Char
type CharGrid = A.Array GridPos Char

wordsWhen :: (a -> Bool) -> [a] -> [[a]]
wordsWhen p s =
  case dropWhile p s of
    [] -> []
    s' -> w : wordsWhen p s''
     where
      (w, s'') = break p s'

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn c = wordsWhen (== c)

trimIf :: (a -> Bool) -> [a] -> [a]
trimIf p = reverse . (dropWhile p) . reverse . (dropWhile p)

trimChar :: (Eq a) => a -> [a] -> [a]
trimChar c = trimIf (== c)

trimSpace :: String -> String
trimSpace = trimIf (== ' ')

readStrList :: (Read a, Num a) => Char -> String -> [a]
readStrList delim str = read <$> splitOn delim str

findSublist :: (Eq a) => [a] -> [a] -> Maybe Int
findSublist subList ls = findIndex (isPrefixOf subList) (tails ls)

splitBySubstr :: (Eq a) => [a] -> [a] -> [[a]]
splitBySubstr delim str =
  case findSublist delim str of
    Nothing -> [str]
    Just idx ->
      take idx str : splitBySubstr delim (drop (idx + length delim) str)

consecutivePairs :: [a] -> Maybe [(a, a)]
consecutivePairs (x0 : x1 : xs) = ((x0, x1) :) <$> consecutivePairs xs
consecutivePairs [_] = Nothing
consecutivePairs [] = Just []

writeXY :: (Show a) => String -> [a] -> [a] -> IO ()
writeXY fileName xs ys = do
  writeFile fileName $
    intercalate "\n" $
      zipWith (\x y -> show x <> ", " <> show y) xs ys

groupByUnique :: (Ord b) => (a -> b) -> [a] -> [(a, Int)]
groupByUnique ordFunc ls = map (\grp -> (fst $ head grp, length grp)) (groupBy ((==) `on` snd) sortedList)
 where
  sortedList = sortOn snd $ map (\x -> (x, ordFunc x)) ls

-- groupBySorted :: (Ord b) => (a->b) -> [a] -> [([a],b)]
groupBySorted ordFunc ls = map (\grp -> (fst <$> grp, snd . head $ grp)) $ groupBy ((==) `on` snd) sortedList
 where
  sortedList = sortOn snd $ zip ls mappedList
  mappedList = ordFunc <$> ls

countIf :: (a -> Bool) -> [a] -> Int
countIf = (length .) . filter

pairChoices :: [a] -> [(a, a)]
-- pairChoices xs = concat $ zipWith (\a rest -> [(a, r) | r<-rest] ) xs (tail $ tails xs) -- [(xs !! i, xs !! j) | i <- [0 .. length xs - 1], j <- [i + 1 .. length xs - 1]]
pairChoices = concat . (zipWith (map . (,)) <*> (tail . tails)) -- just for point free fun, more comprehensible  implementations above
pairVariations :: [a] -> [(a, a)]
-- pairVariations xs = [(xs !! i, xs !! j) | i <- [0 .. length xs - 1], j <- [0 .. length xs - 1], i /= j]
pairVariations = ((++) <*> map swap) . pairChoices

strToCharGrid :: (A.IArray a Char) => String -> a GridPos Char
strToCharGrid file = A.listArray ((1, 1), (numLines, lineSize)) $ concat ls
 where
  ls = lines file
  numLines = length ls
  lineSize = length $ head ls

charGridToStr :: (A.IArray a Char) => a GridPos Char -> [String]
charGridToStr charGrid = [[charGrid A.! (y, x) | x <- [xmin .. xmax]] | y <- [ymin .. ymax]]
 where
  ((ymin, xmin), (ymax, xmax)) = A.bounds charGrid
