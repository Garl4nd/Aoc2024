{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module N9 (getSolutions9) where

import Control.Applicative (Alternative (empty))
import Control.Arrow
import Control.Monad ((>=>))
import Data.Char (digitToInt)
import Data.Foldable (Foldable (toList))
import Data.Sequence (Seq ((:<|), (:|>)), ViewL ((:<)), (><))
import qualified Data.Sequence as S
import Prelude hiding (id)

type ID = Int
data Block = IdBlock {id :: ID, filledSize :: Int} | FreeBlock {freeSize :: Int}
type Disk = [Block]

instance Show Block where
  show IdBlock{id, filledSize} = (concat . replicate filledSize) $ show id
  show FreeBlock{freeSize} = replicate freeSize '.'

parseFile :: String -> Disk
parseFile file = fillDisk 0 $ digitToInt <$> numberList
 where
  numberList = init file
  fillDisk :: ID -> [Int] -> Disk
  fillDisk _ [] = []
  fillDisk id [size] = [IdBlock{id, filledSize = size}]
  fillDisk id (filledSize : freeSize : rest) = IdBlock{id, filledSize} : FreeBlock{freeSize} : fillDisk (id + 1) rest

printDisk :: Disk -> String
printDisk = concatMap show

isFree :: Block -> Bool
isFree (FreeBlock _) = True
isFree _ = False

moveLastBlock :: Disk -> Maybe (Disk, Disk)
moveLastBlock disk = case dropWhile isFree . reverse $ disk of
  [] -> Nothing
  (IdBlock lastId lastSize) : restOfDisk ->
    let (processed, rest) = go (lastId, lastSize) ([], reverse restOfDisk) in Just (reverse processed, rest)
   where
    go :: (ID, Int) -> (Disk, Disk) -> (Disk, Disk)
    go (id, size) (processed, []) = (IdBlock id size : processed, [])
    go (id, remSize) (processed, FreeBlock{freeSize} : rest)
      | remSize <= freeSize =
          ( IdBlock{id, filledSize = remSize} : processed
          , if remSize == freeSize then rest else FreeBlock{freeSize = freeSize - remSize} : rest
          )
      | otherwise = go (id, remSize - freeSize) (IdBlock{id, filledSize = freeSize} : processed, rest)
    go blockTup (processed, idBlock : rest) = go blockTup (idBlock : processed, rest)

rearrangeDisk :: Disk -> Disk
rearrangeDisk disk = case moveLastBlock disk of
  Nothing -> disk
  Just (processed, rest) -> processed ++ rearrangeDisk rest

rearrangeDisk2 :: Disk -> Disk
rearrangeDisk2 disk = go (disk, [])
 where
  go :: (Disk, Disk) -> Disk
  go (unprocessed, processed) = case span isFree . reverse $ unprocessed of
    (_, []) -> processed
    (revEnd, block : revLd) ->
      let (end, ld) = (reverse revEnd, reverse revLd)
       in case tryInsertBlock ld block of
            Just modifiedLd -> go (modifiedLd, FreeBlock{freeSize = filledSize block} : end ++ processed)
            Nothing -> go (ld, block : end ++ processed)
  tryInsertBlock :: Disk -> Block -> Maybe Disk
  tryInsertBlock _ (FreeBlock _) = Nothing
  tryInsertBlock disk block@IdBlock{filledSize} = case break (\block' -> isFree block' && freeSize block' >= filledSize) disk of
    (_, []) -> Nothing
    (start, FreeBlock{freeSize} : rest) -> Just $ start ++ block : FreeBlock{freeSize = freeSize - filledSize} : rest

rearrangeDisk2Seq :: S.Seq Block -> S.Seq Block
rearrangeDisk2Seq disk = go (disk, S.empty)
 where
  go :: (S.Seq Block, S.Seq Block) -> S.Seq Block
  go (unprocessed, processed) = case S.spanr isFree unprocessed of -- span isFree . reverse $ unprocessed of
    (_, S.viewl -> S.EmptyL) -> processed
    (end, ld :|> block) -> case tryInsertBlock ld block of
      Just modifiedLd -> go (modifiedLd, FreeBlock{freeSize = filledSize block} :<| end >< processed)
      Nothing -> go (ld, block :<| end >< processed)
  tryInsertBlock :: S.Seq Block -> Block -> Maybe (S.Seq Block)
  tryInsertBlock _ (FreeBlock _) = Nothing
  tryInsertBlock disk block@IdBlock{filledSize} = case S.breakl (\block' -> isFree block' && freeSize block' >= filledSize) disk of
    (_, S.viewl -> S.EmptyL) -> Nothing
    (start, FreeBlock{freeSize} :<| rest) -> Just $ start >< block :<| FreeBlock{freeSize = freeSize - filledSize} :<| rest

rearrangeDisk2' = toList . rearrangeDisk2Seq . S.fromList

unwrapDisk :: Disk -> [Maybe ID]
unwrapDisk =
  concatMap
    ( \case
        FreeBlock{freeSize} -> replicate freeSize Nothing
        IdBlock{id, filledSize} -> replicate filledSize (Just id)
    )

checkSum :: [Maybe ID] -> Int
checkSum =
  sum
    . zipWith
      ( \pos maybeId -> case maybeId of
          Nothing -> 0
          Just id -> pos * id
      )
      [0 ..]

solution1 :: Disk -> Int
solution1 = rearrangeDisk >>> unwrapDisk >>> checkSum

solution2 :: Disk -> Int
solution2 = rearrangeDisk2' >>> unwrapDisk >>> checkSum

getSolutions9 :: String -> IO (Int, Int)
getSolutions9 = readFile >=> (parseFile >>> (solution1 &&& solution2) >>> return)
