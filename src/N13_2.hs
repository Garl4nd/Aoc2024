{-# LANGUAGE NamedFieldPuns #-}

module N13_2 () where

import Control.Monad (when)
import Data.Either (fromRight)
import Data.Maybe (catMaybes)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

type MyField = Double
type SParser = Parsec Void String
type NumPair = (Int, Int)
data Equation = Equation {u :: NumPair, v :: NumPair, b :: NumPair} deriving (Show)

eqParser :: SParser Equation
eqParser =
  let
    vecParser :: String -> SParser NumPair
    vecParser sign = do
      a1 <- string (": X" <> sign) *> L.decimal
      a2 <- string (", Y" <> sign) *> L.decimal
      return (a1, a2)
   in
    do
      u <- string "Button A" *> vecParser "+" <* newline
      v <- string "Button B" *> vecParser "+" <* newline
      b <- string "Prize" *> vecParser "=" <* newline
      return Equation{u, v, b}

parseFile :: String -> [Equation]
parseFile file = fromRight [] $ runParser (sepEndBy eqParser newline) "" file

doubleSolution :: Equation -> (Maybe [Int], Maybe [Int])
doubleSolution Equation{u = (u1, u2), v = (v1, v2), b = (b1, b2)} = (singleSolution u1 v1 b1, singleSolution u2 v2 b2)
singleSolution :: Int -> Int -> Int -> Maybe [Int]
singleSolution a b r =
  let (d, rem) = r `quotRem` (gcd a b)
   in if rem /= 0
        then Nothing
        else Just [d, 2 * d .. r]

solveEq :: Equation -> Maybe NumPair
solveEq eq = Nothing
