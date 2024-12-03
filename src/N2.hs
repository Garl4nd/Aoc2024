module N2 (getSolutions2) where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import Control.Arrow
import Control.Monad ((>=>))
import Data.Either (fromRight)
import Data.Maybe (isJust)
import Useful (countIf)
import Data.List (inits, tails)
type SParser = Parsec Void String
parseFile :: String -> [[Int]]
parseFile file =
  let
    lineParser :: SParser [Int]
    lineParser = sepBy L.decimal (char ' ')     
    fileParser = sepBy lineParser newline
   in
   fromRight [] $ runParser fileParser "" file

(<||>) = liftA2 (||)
--(<&&>) = liftA2 (&&)  

isSafe :: [Int] -> Bool
isSafe  = isSafeForward <||> (isSafeForward . reverse) where 
    difInRange l r = (l+1<= r) && (r<=l+3)
    isSafeForward [] = False
    isSafeForward report = and $ zipWith  difInRange ( drop 1 report)  report        

isSafe' :: Int -> [Int] -> Bool
isSafe' lives = isSafeForward lives  <||> (isSafeForward lives . reverse) where
  isSafeForward :: Int -> [Int]  -> Bool  
  isSafeForward _ [] = False
  isSafeForward lives (x:xs)  = isJust $ foldr ((=<<).check)  (Just (x, lives)) $ reverse xs
  check cNum (lastNum, lives) 
    | inRange cNum lastNum = Just (cNum, lives) 
    | lives >1 = Just (lastNum, lives -1) 
    | otherwise = Nothing
  inRange l r = r>=l+1 && r<=l+3

solution1 :: [[Int]] -> Int
solution1 = countIf (isSafe' 1)

solution2 :: [[Int]] -> Int
solution2 = countIf (isSafe' 2) -- countIf (isSafe  <||> (any isSafe .  augmented)) where
  --augmented report = zipWith (++) (inits report) (tail $ tails report ) 
  -- augmented report = [remove n report | n <- [1..length report]]  
  -- remove n report = let (l, r) = splitAt (n-1) report in l ++ drop 1 r
-- \cNum acc -> check cNum =<< acc
getSolutions2 :: String -> IO (Int, Int)
getSolutions2 = readFile >=> (parseFile >>> (solution1 &&& solution2) >>> return)
