{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Trie where

import qualified Data.Map as M
import Data.Maybe (maybeToList)

data (Ord k) => Trie k v = Node {val :: Maybe v, trieMap :: TrieMap k v} deriving (Show)
type TrieMap k v = M.Map k (Trie k v)

emptyTrie :: (Ord k) => Trie k v
emptyTrie = Node Nothing M.empty

insertWith :: forall k v. (Ord k) => (v -> k -> v) -> v -> [k] -> Trie k v -> Trie k v
insertWith _ _ [] = id
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

insertWithConst :: forall k v. (Ord k) => v -> [k] -> Trie k v -> Trie k v
insertWithConst _ [] = id
insertWithConst val ks = go ks
 where
  go :: [k] -> Trie k v -> Trie k v
  go [] node = node{val = Just val}
  go (key : rest) node@Node{trieMap} = case M.lookup key trieMap of
    Just trie -> node{trieMap = modifiedMap}
     where
      modifiedMap = M.insert key modifiedTrie trieMap
      modifiedTrie = go rest trie
    Nothing -> node{trieMap = M.insert key (go rest emptyTrie) trieMap}

insert :: (Ord k) => [k] -> Trie k [k] -> Trie k [k]
insert = insertWith (\accum key -> accum ++ [key]) []

insertWord :: (Ord k) => [k] -> v -> Trie k v -> Trie k v
insertWord word translation = insertWithConst translation word

fromAssocList :: (Ord k) => [([k], v)] -> Trie k v
fromAssocList = foldr (\(word, trans) trie -> insertWord word trans trie) emptyTrie

fromList :: (Ord k) => [[k]] -> Trie k [k]
fromList = foldr insert emptyTrie

fromListWith :: (Ord k) => (v -> k -> v) -> v -> [[k]] -> Trie k v
fromListWith f acc = foldr (insertWith f acc) emptyTrie

instance (Ord k) => Semigroup (Trie k v) where
  trieA <> trieB = foldr (uncurry insertWord) trieA $ toAssocList trieB
instance (Ord k) => Monoid (Trie k v) where
  mempty = emptyTrie
instance (Ord k) => Functor (Trie k) where
  fmap f Node{val, trieMap} = Node{trieMap = fmap f <$> trieMap, val = f <$> val}
toList :: forall k v. (Ord k) => Trie k v -> [v]
toList Node{val, trieMap} = maybeToList val ++ concatMap toList (M.elems trieMap)

toAssocList :: forall k v. (Ord k) => Trie k v -> [([k], v)]
toAssocList = go [] -- maybeToList (([],) <$> val) ++ concat [go [k] trie | (k, trie) <- M.assocs trieMap] where
 where
  go :: [k] -> Trie k v -> [([k], v)]
  go kAcc Node{val, trieMap} = maybeToList ((kAcc,) <$> val) ++ concat ([go (kAcc ++ [key]) trie | (key, trie) <- M.assocs trieMap])

findSubtrie :: (Ord k) => Trie k v -> [k] -> Maybe (Trie k v)
findSubtrie trie [] = Just trie
findSubtrie Node{trieMap} (key : rest) =
  case M.lookup key trieMap of
    Just trie -> findSubtrie trie rest
    Nothing -> Nothing

findByKey :: (Ord k) => Trie k v -> [k] -> Maybe v
findByKey = ((val =<<) .) . findSubtrie

suffixAssocs :: (Ord k) => Trie k v -> [k] -> [([k], v)]
suffixAssocs trie ks = maybe [] toAssocList $ findSubtrie trie ks
