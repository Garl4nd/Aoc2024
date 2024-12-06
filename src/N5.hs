module N5 (getSolutions5) where

import Control.Arrow
import Control.Monad ((>=>))
import Data.Either (fromRight)
import Data.List (partition)
import qualified Data.Map as M
import Data.Maybe (isJust)
import qualified Data.Set as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

type OrderMap = M.Map Int [Int]
type Record = [Int]
type SParser = Parsec Void String

parseFile :: String -> (OrderMap, [Record])
parseFile file = fromRight (M.empty, []) $ runParser fileParser "" file
 where
  fileParser :: SParser (OrderMap, [Record])
  fileParser = do
    orderMap <- pairListToOrderMap <$> endBy ((,) <$> (L.decimal <* char '|') <*> L.decimal) newline <* newline
    records <- endBy (sepBy L.decimal $ char ',') newline
    return (orderMap, records)
  pairListToOrderMap :: [(Int, Int)] -> OrderMap
  pairListToOrderMap = foldr (\(k, v) m -> M.insertWith (++) k [v] m) M.empty

isCorrectRecord :: OrderMap -> Record -> Bool
isCorrectRecord orderMap = isJust . foldr ((=<<) . checkAndUpdateForbidden) (Just S.empty)
 where
  checkAndUpdateForbidden :: Int -> S.Set Int -> Maybe (S.Set Int)
  checkAndUpdateForbidden current forbiddenSet
    | current `S.member` forbiddenSet = Nothing
    | otherwise = case M.lookup current orderMap of
        Nothing -> Just forbiddenSet
        Just nums -> Just $ S.union forbiddenSet (S.fromList nums)

middleSum :: [Record] -> Int
middleSum = sum . map (\rec -> rec !! (length rec `div` 2))

solution1 :: (OrderMap, [Record]) -> Int
solution1 (orderMap, records) =
  let
    correctRecords = filter (isCorrectRecord orderMap) records
   in
    middleSum correctRecords

fixRecord :: OrderMap -> Record -> Record
fixRecord orderMap = foldr addNumToRecord []
 where
  addNumToRecord :: Int -> Record -> Record
  addNumToRecord num record =
    let
      (predecessors, rest) = partition (\k -> num `elem` M.findWithDefault [] k orderMap) record
     in
      predecessors ++ num : rest

solution2 :: (OrderMap, [Record]) -> Int
solution2 (orderMap, records) =
  let
    incorrectRecords = filter (not . isCorrectRecord orderMap) records
    fixedRecords = fixRecord orderMap <$> incorrectRecords
   in
    middleSum fixedRecords

getSolutions5 :: String -> IO (Int, Int)
getSolutions5 = readFile >=> (parseFile >>> (solution1 &&& solution2) >>> return)
