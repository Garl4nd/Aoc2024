module N2 (getSolutions2) where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

import Control.Arrow
import Control.Monad ((>=>))
import Data.Either (fromRight)
import Data.Function (on)
import Data.List (sort)
import Useful (countIf)

-- type TParser = Parsec Void T.Text
type SParser = Parsec Void String

parseFile :: String -> [[Int]]
parseFile file =
  let
    lineParser :: SParser [Int]
    lineParser = sepBy L.decimal (char ' ')     
    fileParser = sepEndBy lineParser newline
   in
    fromRight [] $ runParser fileParser "" file

solution1 :: [[Int]] -> Int
solution1 ls =
  let    
    between x l u  = (x>=l ) and (x<=r)
    isSafeForward report = all $ zipWith ( \l r -> r - l `between` 1 3 )  report $ drop 1 report        
    isSafe report = isSafeForward report or isSafeForward (reverse report) 
   in
    length $ filter isSafe ls

--    (lefts, rights) = join (***) sort $ unzip ls -- (map fst &&& map snd) ls
-- (lefts, rights) = (sort *** sort) $ unzip ls -- (map fst &&& map snd) ls
-- (lefts, rights) = uncurry ((,) `on` sort) $ unzip ls

-- sum $ zipWith ((abs .) . subtract) lefts rights
--  sum $ zipWith (subtract >>> (abs .)) lefts rights
-- sum $ zipWith (subtract >>> (>>>abs)) lefts rights

solution2 :: [(Int, Int)] -> Int
solution2 ls =
  let
    count target = countIf (== target)
    (lefts, rights) = unzip ls
   in
    sum $ [l * count l rights | l <- lefts]

-- >>> print $ readFile "inputs/2.txt"
-- >>> parseFile <$> readFile "inputs/2.txt"
-- <stderr>: hPutChar: invalid argument (cannot encode character '\8216')

getSolutions2 :: String -> IO (Int, Int)
getSolutions2 = readFile >=> (parseFile >>> (solution1 &&& const 0) >>> return)
  -- const $ return (0,0) --  readFile >=> (parseFile >>> (solution1 &&& solution2) >>> return)
