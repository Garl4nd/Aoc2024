{-# LANGUAGE BangPatterns #-}

module N22 (getSolutions22) where

import Control.Arrow
import Control.Monad (forM_, when, (>=>))
import Control.Parallel.Strategies
import Data.Array.ST (MArray (newArray), readArray, runSTUArray, writeArray)
import qualified Data.Array.Unboxed as A
import Data.Bits (Bits (xor))
import Data.Foldable (Foldable (toList))
import Data.Function ((&))
import Data.List (mapAccumL, nub, scanl', tails)
import qualified Data.Set as S

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

encDifSeqDict :: Int -> [(Int, Int)]
encDifSeqDict n =
  let
    encodeSign = (9 +)
    encodeSeq [a, b, c, d] = 19 ^ 3 * a + 19 ^ 2 * b + 19 * c + d
    encode = encodeSeq . map encodeSign
    quadruplets = take 4 <$> tails (difAndDigitSeqs n)
    difValPairs quadruplet = let (difs, vals) = unzip quadruplet in (encode difs, last vals)
   in
    take (2000 - 3) $ difValPairs <$> quadruplets

encDifSeqDict' :: Int -> [(Int, Int)]
encDifSeqDict' n =
  let
    encodeSign = (9 +)
    encodeSeq [a, b, c, d] = 19 ^ 3 * a + 19 ^ 2 * b + 19 * c + d
    encodeNew current new = (current * 19 + (encodeSign new)) `mod` 19 ^ 4
    encode = encodeSeq . map encodeSign
    (firstQuadruplet, rest) = splitAt 4 (difAndDigitSeqs n)
    firstPair = (encode $ fst <$> firstQuadruplet, last $ snd <$> firstQuadruplet)
    -- difSeqDicts = mapAccumL (\currentCode (dif, val) -> let newCode = encodeNew currentCode dif in (newCode, (newCode, val))
    difSeqDicts = scanl' (\(currentCode, _) (dif, val) -> (encodeNew currentCode dif, val)) firstPair rest
   in
    -- scanl (\)

    take (2000 - 3) difSeqDicts

-- take (2000 - 3) $ difValPairs <$> quadruplets

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

makeArray' :: [(Int, Int)] -> A.UArray Int Int
makeArray' encDict = runSTUArray $ do
  ar <- newArray (0, 19 ^ 4 - 1) 0
  forM_ encDict $ \(index, val) -> do
    current <- readArray ar index
    when (current == 0) $ writeArray ar index val
  return ar

solution1 :: [Int] -> Int
solution1 nums = sum secretNums
 where
  secretNums = [sqn !! 2000 | sqn <- genSequence <$> nums]

solution2' :: [Int] -> Int
solution2' nums =
  let
    encDicts = (encDifSeqDict' <$> nums) `using` parListChunk 500 rdeepseq
    arrays = (makeArray' <$> encDicts) `using` parListChunk 500 rseq
    seqScores = [sum [array A.! i | array <- arrays] | i <- [0 .. 19 ^ 4 - 1]] `using` parListChunk 500 rdeepseq
   in
    maximum seqScores --

--  seqScoreMaximum encDict = maximum ([sum [array A.! i | array <- arrays] | (i, _) <- encDict])
--  seqScoreMaxima = map seqScoreMaximum encDicts `using` parListChunk 500 rdeepseq
-- in
--  maximum seqScoreMaxima

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
getSolutions22 = readFile >=> (parseFile >>> (const 0 &&& solution2') >>> return)
