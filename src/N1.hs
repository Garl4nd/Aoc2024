module N1 (getSolutions1) where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

import Control.Arrow
import Data.Either (fromRight)
import Data.List (sort)
import Useful (countIf)

-- type TParser = Parsec Void T.Text
type SParser = Parsec Void String

parseFile :: String -> [(Int, Int)]
parseFile file =
  let
    lineParser :: SParser (Int, Int)
    lineParser = do
      num1 <- L.decimal
      num2 <- many space1 >> L.decimal
      return (num1, num2)
    fileParser = sepEndBy lineParser newline
   in
    fromRight [] $ runParser fileParser "" file

solution1 :: [(Int, Int)] -> Int
solution1 ls =
  let
    (lefts, rights) = sort <$> unzip ls -- (map fst &&& map snd) ls
   in
    sum $ zipWith (\l r -> abs (r - l)) lefts rights

solution2 :: [(Int, Int)] -> Int
solution2 ls =
  let
    count target = countIf (== target)
    (lefts, rights) = unzip ls
   in
    sum $ [l * count l rights | l <- lefts]

-- >>> solution1 . parseFile <$> readFile "inputs/1.txt"
-- 30311288
-- >>> solution2 . parseFile <$> readFile "inputs/1.txt"
-- 23529853
getSolutions1 :: String -> IO (Int, Int)
getSolutions1 filename = readFile filename >>= (parseFile >>> (solution1 &&& solution2) >>> return)
