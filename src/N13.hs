{-# LANGUAGE NamedFieldPuns #-}

module N13 (getSolutions13) where

import Control.Arrow
import Control.Monad (guard, (>=>))
import Data.Either (fromRight)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Void (Void)
import Numeric.LinearAlgebra hiding ((<>))
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

type SParser = Parsec Void String
data Equation = Equation {u :: Vector Double, v :: Vector Double, b :: Vector Double} deriving (Show)

eqParser :: SParser Equation
eqParser =
  let
    vecParser :: String -> SParser (Vector Double)
    vecParser sign = do
      a1 <- string (": X" <> sign) *> L.decimal
      a2 <- string (", Y" <> sign) *> L.decimal
      return $ fromList [a1, a2]
   in
    do
      u <- string "Button A" *> vecParser "+" <* newline
      v <- string "Button B" *> vecParser "+" <* newline
      b <- string "Prize" *> vecParser "=" <* newline
      return Equation{u, v, b}

parseFile :: String -> [Equation]
parseFile file = fromRight [] $ runParser (sepEndBy eqParser newline) "" file

getPushCounts :: Equation -> Maybe (Vector Double)
getPushCounts Equation{u, v, b} =
  let
    mA = fromColumns [u, v]
    mB = fromColumns [b]
    solutionMatrix = linearSolve mA mB
   in
    do
      solMatrix <- solutionMatrix
      solVec <- listToMaybe $ toColumns solMatrix
      guard $ mA #> roundVector solVec == flatten mB -- integerSolution ?
      return solVec

solution1 :: [Equation] -> Int
solution1 = sum . map tokenCount . mapMaybe getPushCounts
 where
  tokenCount pushes = round $ vector [3, 1] <.> pushes

solution2 :: [Equation] -> Int
solution2 = solution1 . map modifyEq
 where
  modifyEq eq@Equation{b} = eq{b = b + 10000000000000}

getSolutions13 :: String -> IO (Int, Int)
getSolutions13 = readFile >=> (parseFile >>> (solution1 &&& solution2) >>> return)
