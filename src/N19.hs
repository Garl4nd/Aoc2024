{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module N19 (getSolutions19) where

import Control.Arrow
import Control.Monad ((>=>))
import Data.Function.Memoize (Memoizable, memoFix)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Useful (countIf, readStrList, splitBySubstr, trimSpace)

type TrieMap k v = M.Map k (Trie k v)

data Trie k v = Node {val :: Maybe v, trieMap :: (TrieMap k v)} deriving (Show)

parseInput :: String -> ([String], [String])
parseInput file =
  let [p1, p2] = splitBySubstr "\n\n" file
   in (trimSpace <$> splitBySubstr "," p1, lines p2)
emptyTrie :: Trie k v
emptyTrie = Node Nothing M.empty

insertWith :: forall k v. (Ord k) => (v -> k -> v) -> v -> [k] -> Trie k v -> Trie k v
insertWith f acc [] = id
insertWith f acc ks = go acc ks
 where
  go :: v -> [k] -> Trie k v -> Trie k v
  go accum [] node = node{val = Just accum}
  go accum (key : rest) node@Node{trieMap} = case M.lookup key trieMap of
    Just trie -> node{trieMap = modifiedMap}
     where
      modifiedMap = M.insert key modifiedTrie trieMap
      modifiedTrie = go (accum `f` key) rest trie
    Nothing -> node{trieMap = M.insert key (go (accum `f` key) rest emptyTrie) trieMap}

insert :: (Ord k) => [k] -> Trie k [k] -> Trie k [k]
insert = insertWith (\accum key -> accum ++ [key]) []

fromList :: (Ord k) => [[k]] -> Trie k [k]
fromList ks = foldr insert emptyTrie ks

fromListWith :: (Ord k) => (v -> k -> v) -> v -> [[k]] -> Trie k v
fromListWith f acc ks = foldr (insertWith f acc) emptyTrie ks

toList :: forall k v. (Ord k) => Trie k v -> [v]
toList Node{val, trieMap} = maybeToList val ++ (concatMap toList $ M.elems trieMap)

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
