module N3 (getSolutions3) where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import Control.Arrow
import Control.Monad ((>=>), void)
import Text.Megaparsec.Debug
import Data.Either (fromRight)
import Data.Maybe ( catMaybes)
type SParser = Parsec Void String

exprParser1 :: SParser  [(Int, Int)]
exprParser1 =   catMaybes <$> manyTill (skipManyTill anySingle $ Nothing <$ try eof <|> Just <$> try mulParser) eof

exprParser2 :: SParser  [(Int, Int)]
exprParser2 =  catMaybes <$> manyTill (skipManyTill anySingle 
        ( Nothing <$ try ignoredInstructions <|> Nothing <$ try eof <|> Just <$> try mulParser)) eof  where 
        ignoredInstructions =  string "don't()" >> skipManyTill anySingle (string "do()")  

mulParser :: SParser (Int, Int)
mulParser =  do 
    void $ string "mul("
    f <- L.decimal 
    s <- char ',' >> L.decimal
    void $ char ')'
    return (f,s)
 
parseFile1 :: String -> [(Int, Int)]
parseFile1 file = fromRight [] $ runParser exprParser1 "" file

parseFile2 :: String -> [(Int, Int)]
parseFile2 file = fromRight [] $ runParser exprParser2 "" file

solution :: [(Int,Int)] -> Int
solution = sum . map (uncurry (*))

getSolutions3 :: String -> IO (Int, Int)
getSolutions3 = readFile >=> (( (parseFile1 >>> solution) &&& (parseFile2 >>> solution)) >>> return)
