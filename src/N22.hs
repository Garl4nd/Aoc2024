module N22 (getSolutions22) where

import Control.Arrow
import Control.Monad ((>=>))
import Data.Bits (Bits (xor))
import Data.Function ((&))
import Data.List (nub, tails)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Trie

thenMixPrune :: (Int -> Int) -> (Int -> Int)
thenMixPrune f = mix <*> f >>> prune
secretMult :: Int -> Int
secretMult = (* 64) & thenMixPrune

secretMult2 :: Int -> Int
secretMult2 = (* 2048) & thenMixPrune

secretDiv :: Int -> Int
secretDiv = (`div` 32) & thenMixPrune
genSeq :: Int -> Int
genSeq = secretMult >>> secretDiv >>> secretMult2

mix = xor
prune = (`mod` 16777216)

simulateNumbers :: Int -> [Int]
simulateNumbers = iterate genSeq

seqAndDifs :: Int -> [(Int, Int)]
seqAndDifs n =
  let
    seq = (`mod` 10) <$> simulateNumbers n
    difs = zipWith subtract seq (tail seq)
   in
    zip (tail seq) difs

-- difSeqDict :: Int -> [([Int], Int)]
difSeqDict n =
  let
    fours = take 4 <$> tails (seqAndDifs n)
    difValPairs group = (snd <$> group, last $ fst <$> group)
   in
    take (2000 - 3) $ difValPairs <$> fours

type SeqTrie = Trie Int Int

makeSeqTrie :: Int -> SeqTrie
makeSeqTrie = fromAssocList . difSeqDict

makeSeqTries :: [Int] -> [SeqTrie]
makeSeqTries = map makeSeqTrie

possibleSeqs :: SeqList
possibleSeqs = [[a, b, c, d] | let r = [-9 .. 9], a <- r, b <- r, c <- r, d <- r]
type SeqList = [[Int]]
getAllDifSeqs :: [SeqTrie] -> SeqList
getAllDifSeqs tries =
  let
    getSeqs = map (map fst . toAssocList)
   in
    S.toList $ S.fromList $ concat $ getSeqs tries

score :: [SeqTrie] -> [Int] -> Int
score tries seq = sum $ scoreSingle <$> tries
 where
  scoreSingle = fromMaybe 0 . (`findByKey` seq)

maxScore :: SeqList -> [SeqTrie] -> Int
maxScore seqs tries = maximum $ (score tries) <$> seqs

solution1 :: [Int] -> Int
solution1 nums = sum sNums
 where
  sNums = map (\n -> simulateNumbers n !! 2000) nums

solution2 :: [Int] -> Int
solution2 = maxScore possibleSeqs . makeSeqTries

parseFile :: String -> [Int]
parseFile = map read . lines

getSolutions22 :: String -> IO (Int, Int)
getSolutions22 = readFile >=> (parseFile >>> (solution1 &&& solution2) >>> return)
