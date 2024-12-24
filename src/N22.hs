{-# LANGUAGE BangPatterns #-}

module N22 (getSolutions22) where

import Control.Arrow
import Control.Monad (forM_, unless, when, (>=>))
import Control.Parallel.Strategies
import Data.Array.ST (MArray (newArray), readArray, runSTUArray, writeArray)
import qualified Data.Array.Unboxed as A
import Data.Bits (Bits (xor))
import Data.Function ((&))
import Data.List (nub, tails)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Trie as T

mix f = xor <*> f
prune = (`mod` 16777216)

thenMix'nPrune :: (Int -> Int) -> (Int -> Int)
thenMix'nPrune f = mix f >>> prune

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

difAndDigitSeqs :: Int -> [(Int, Int)]
difAndDigitSeqs n =
  let
    digitSeq = (`mod` 10) <$> genSequence n
    difs = zipWith subtract digitSeq (tail digitSeq)
   in
    zip difs (tail digitSeq)

difSeqDict :: Int -> [([Int], Int)]
difSeqDict n =
  let
    quadruplets = take 4 <$> tails (difAndDigitSeqs n)
    difValPairs quadruplet = let (difs, vals) = unzip quadruplet in (difs, last vals)
   in
    take (2000 - 3) $ difValPairs <$> quadruplets

makeArray :: Int -> A.UArray Int Int
makeArray n =
  let
    dict = difSeqDict n
    encodeSign = (9 +)
    encodeSeq [a, b, c, d] = 19 ^ 3 * a + 19 ^ 2 * b + 19 * c + d
    encode = encodeSeq . map encodeSign
   in
    runSTUArray $ do
      ar <- newArray (0, 19 ^ 4 - 1) 0
      forM_ dict $ \(sqn, val) -> do
        let index = encode sqn
        current <- readArray ar index
        when (current == 0) $ writeArray ar index val
      return ar

solution1 :: [Int] -> Int
solution1 nums = sum secretNums
 where
  secretNums = [sqn !! 2000 | sqn <- genSequence <$> nums]

solution2 :: [Int] -> Int
solution2 nums =
  let
    arrays = (makeArray <$> nums) `using` parListChunk 200 rseq
    seqScores = [sum [array A.! i | array <- arrays] | i <- A.indices (head arrays)] `using` parListChunk 200 rdeepseq
   in
    maximum seqScores --
parseFile :: String -> [Int]
parseFile = map read . lines

getSolutions22 :: String -> IO (Int, Int)
getSolutions22 = readFile >=> (parseFile >>> (solution1 &&& solution2) >>> return)
