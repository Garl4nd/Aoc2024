{-# LANGUAGE NamedFieldPuns #-}

module N9 (getSolutions9) where

import Control.Arrow
import Control.Monad ((>=>))
import Data.Char (digitToInt)
import Useful (countIf)
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
  fillDisk id [] = []
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
rearrangeDisk disk = go disk
 where
  go disk = case moveLastBlock disk of
    Nothing -> disk
    Just (processed, rest) -> processed ++ rearrangeDisk rest

rearrangeDiskWhole :: Disk -> Disk
rearrangeDiskWhole disk = foldr tryToMove disk idList
 where
  idList = [0 .. countIf (not . isFree) disk - 1]
  isId _ (FreeBlock _) = False
  isId blockId (IdBlock{id}) = id == blockId
  tryToMove :: ID -> Disk -> Disk
  tryToMove blockId disk =
    let (start, block : rest) = break (isId blockId) disk
     in case insertBlock start block of
          Just modifiedStart -> modifiedStart ++ FreeBlock{freeSize = filledSize block} : rest
          Nothing -> disk
  insertBlock :: Disk -> Block -> Maybe Disk
  insertBlock _ (FreeBlock _) = Nothing
  insertBlock disk block@IdBlock{id, filledSize} = case break (\block -> isFree block && freeSize block >= filledSize) disk of
    (_, []) -> Nothing
    (start, FreeBlock{freeSize = freeSize'} : rest) -> Just $ start ++ block : FreeBlock{freeSize = freeSize' - filledSize} : rest

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
solution2 = rearrangeDiskWhole >>> unwrapDisk >>> checkSum

getSolutions9 :: String -> IO (Int, Int)
getSolutions9 = readFile >=> (parseFile >>> (solution1 &&& const 0) >>> return)
