{-# LANGUAGE BangPatterns #-}

module N22 (getSolutions22) where

import Control.Arrow
import Control.Monad ((>=>))
import qualified Data.Array as A
import Control.Parallel.Strategies
import Data.Bits (Bits (xor))
import Data.Function ((&))
import Data.List (nub, tails)
import qualified Data.Map as M
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

nextNumber :: Int -> Int
nextNumber = secretMult >>> secretDiv >>> secretMult2

genSequence :: Int -> [Int]
genSequence = iterate nextNumber

digitAndDifSeqs :: Int -> [(Int, Int)]
digitAndDifSeqs n =
  let
    digitSeq = (`mod` 10) <$> genSequence n
    difs = zipWith subtract digitSeq (tail digitSeq)
   in
    zip (tail digitSeq) difs

difSeqDict :: Int -> [([Int], Int)]
difSeqDict n =
  let
    quadruplets = take 4 <$> tails (digitAndDifSeqs n)
    difValPairs quadruplet = (snd <$> quadruplet, last $ fst <$> quadruplet)
   in
    take (2000 - 3) $ difValPairs <$> quadruplets

type SeqTrie = Trie Int Int

makeArray :: Int -> A.Array Int Int
makeArray n = let 
  dict = difSeqDict n 
  seqToNum [a, b, c, d] = a*1000+b*100+c*10+d 
  in A.accumArray (const id) 0 (-9999,9999) $ reverse [(seqToNum sqn, val) | (sqn, val) <- dict]

makeSeqTrie :: Int -> SeqTrie
makeSeqTrie = fromAssocList . difSeqDict

possibleSeqs :: SeqList
possibleSeqs = [[a, b, c, d] | let r = [-9 .. 9], a <- r, b <- r, c <- r, d <- r]

type SeqList = [[Int]]
allDifSeqs :: [([Int], Int)] -> S.Set [Int]
allDifSeqs = S.fromList . map fst

score :: [SeqTrie] -> [Int] -> Int
score tries sqn = sum $ scoreSingle <$> tries
 where
  scoreSingle = fromMaybe 0 . (`findByKey` sqn)

maxScore :: SeqList -> [SeqTrie] -> Int
maxScore seqs tries =
  let
    scores = score tries <$> seqs
   in
    maximum (scores `using` parListChunk 64 rdeepseq) -- parScores

solution1 :: [Int] -> Int
solution1 nums = sum sNums
 where
  sNums = map (\n -> genSequence n !! 2000) nums

solution2 :: [Int] -> Int
solution2 nums =
  let
    seqDicts = (difSeqDict <$> nums) `using` parList rdeepseq
    tries = (fromAssocList <$> seqDicts) `using` parList rseq
    seqs = (S.toList $ S.unions $ allDifSeqs <$> seqDicts) `using` parList rdeepseq
   in
    maxScore seqs tries --

solution2' :: [Int] -> Int
solution2' nums =
  let
    arrays = (makeArray <$> nums) `using` parList rdeepseq 
    seqScores =  [sum [array A.! i | array <- arrays] | i <- A.indices (head arrays)] `using` parList rdeepseq  
    in maximum seqScores --
parseFile :: String -> [Int]
parseFile = map read . lines

getSolutions22 :: String -> IO (Int, Int)
getSolutions22 = readFile >=> (parseFile >>> (solution1 &&& solution2') >>> return)
