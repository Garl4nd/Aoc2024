{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module N19 (getSolutions19) where

import Control.Arrow
import Control.Monad ((>=>))
import Data.Function.Memoize (Memoizable, memoFix)
import qualified Data.Map as M
import Data.Maybe (catMaybes, mapMaybe, maybeToList)
import Trie
import Useful (countIf, readStrList, splitBySubstr, trimSpace)

parseInput :: String -> ([String], [String])
parseInput file =
  let [p1, p2] = splitBySubstr "\n\n" file
   in (trimSpace <$> splitBySubstr "," p1, lines p2)
type Memo f = f -> f

allPrefixSufixes :: (Ord k) => Trie k v -> [k] -> [(v, [k])]
allPrefixSufixes _ [] = []
allPrefixSufixes Node{trieMap} (key : rest) =
  case M.lookup key trieMap of
    Just trie@Node{val} -> currentResult ++ allPrefixSufixes trie rest
     where
      currentResult = case val of
        Just prefix -> [(prefix, rest)]
        _ -> []
    Nothing -> []

formable :: forall k v. (Ord k, Memoizable k) => Trie k v -> [k] -> Bool
formable trie = memoFix formableM
 where
  formableM :: Memo ([k] -> Bool)
  formableM _ [] = True
  formableM formableM word = any formableM [sufix | (_, sufix) <- allPrefixSufixes trie word]

numOfDesigns :: forall k v. (Ord k, Memoizable k) => Trie k v -> [k] -> Int
numOfDesigns trie = memoFix countM
 where
  countM :: Memo ([k] -> Int)
  countM _ [] = 1
  countM countM word = sum $ countM <$> [sufix | (_, sufix) <- allPrefixSufixes trie word]

solution1 :: ([String], [String]) -> Int
solution1 (prefixes, words) = let trie = fromList prefixes in countIf (formable trie) words

solution2 :: ([String], [String]) -> Int
solution2 (prefixes, words) = let trie = fromList prefixes in sum $ numOfDesigns trie <$> words

getSolutions19 :: String -> IO (Int, Int)
getSolutions19 = readFile >=> (parseInput >>> (solution1 &&& solution2) >>> return)
