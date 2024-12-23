module N22 (getSolutions22) where

import Control.Arrow
import Control.Monad ((>=>))
import Control.Parallel.Strategies
import Data.Bits (Bits (xor))
import Data.Function ((&))
import Data.List (nub, tails)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Trie

mix = xor
prune = (`mod` 16777216)

thenMix'nPrune :: (Int -> Int) -> (Int -> Int)
thenMix'nPrune f = mix <*> f >>> prune

secretMult :: Int -> Int
secretMult = (* 64) & thenMix'nPrune

secretMult2 :: Int -> Int
secretMult2 = (* 2048) & thenMix'nPrune

secretDiv :: Int -> Int
secretDiv = (`div` 32) & thenMix'nPrune

genSeq :: Int -> Int
genSeq = secretMult >>> secretDiv >>> secretMult2

simulateNumbers :: Int -> [Int]
simulateNumbers = iterate genSeq

seqAndDifs :: Int -> [(Int, Int)]
seqAndDifs n =
  let
    sqn = (`mod` 10) <$> simulateNumbers n
    difs = zipWith subtract sqn (tail sqn)
   in
    zip (tail sqn) difs

difSeqDict :: Int -> [([Int], Int)]
difSeqDict n =
  let
    quadruplets = take 4 <$> tails (seqAndDifs n)
    difValPairs quadruplet = (snd <$> quadruplet, last $ fst <$> quadruplet)
   in
    take (2000 - 3) $ difValPairs <$> quadruplets

type SeqTrie = Trie Int Int

makeSeqTrie :: Int -> SeqTrie
makeSeqTrie = fromAssocList . difSeqDict

possibleSeqs :: SeqList
possibleSeqs = [[a, b, c, d] | let r = [-9 .. 9], a <- r, b <- r, c <- r, d <- r]

type SeqList = [[Int]]
type SeqSet = S.Set [Int]
allDifSeqs :: [([Int], Int)] -> SeqSet
allDifSeqs = S.fromList . map fst
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
maxScore seqs tries =
  let
    scores = score tries <$> seqs
   in
    maximum scores -- `using` parListChunk 100 rdeepseq) -- parScores

solution1 :: [Int] -> Int
solution1 nums = sum sNums
 where
  sNums = map (\n -> simulateNumbers n !! 2000) nums

solution2 :: [Int] -> Int
solution2 nums =
  let
    seqDicts = difSeqDict <$> nums
    tries = fromAssocList <$> seqDicts
    seqs = S.toList $ S.unions $ allDifSeqs <$> seqDicts
   in
    -- parMap = (map makeSeqTrie) `using` parList rdeepseq

    maxScore seqs tries --
    -- maxScore possibleSeqs . map makeSeqTrie $ nums

parseFile :: String -> [Int]
parseFile = map read . lines

getSolutions22 :: String -> IO (Int, Int)
getSolutions22 = readFile >=> (parseFile >>> (solution1 &&& solution2) >>> return)
