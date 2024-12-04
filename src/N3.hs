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
exprParser1 = dbg "exprParser" $ many (skipManyTill anySingle mulParser) 

exprParser2 :: SParser  [(Int, Int)]
exprParser2 = dbg "exprParser" $ catMaybes <$> many (skipManyTill anySingle 
        ( try (Nothing <$ ignoredInstructions) <|> Just <$> mulParser))  where 
        ignoredInstructions =  string "don't()" >> skipManyTill anySingle (string "do()")  

mulParser :: SParser (Int, Int)
mulParser = try $ do 
    void $ string "mul("
    f <- L.decimal 
    s <- char ',' >> L.decimal
    void $ char ')'
    return (f,s)
 
parseFile1 :: String -> [(Int, Int)]
parseFile1 file = fromRight [] $ runParser exprParser1 "" (file <>"mul(0,0)")

parseFile2 :: String -> [(Int, Int)]
parseFile2 file = fromRight [] $ runParser exprParser2 "" (file <>"mul(0,0)")

solution :: [(Int,Int)] -> Int
solution = sum . map (\(l, r) -> l*r) 

getSolutions3 :: String -> IO (Int, Int)
getSolutions3 = readFile >=> (( (parseFile1 >>> solution) &&& (parseFile2 >>> solution)) >>> return)
