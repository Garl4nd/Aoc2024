module N7 (getSolutions7) where
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Data.Either (fromRight)
type SParser = Parsec Void String
type Problem = (Int, [Int])

fileParser :: SParser [Problem]
fileParser = let 
 lineParser = do 
  target <- L.decimal <* string ": "
  nums <- sepEndBy L.decimal $ char ' '
  return (target, nums)
 in sepEndBy lineParser newline 

parseFile :: String -> [Problem]
parseFile file = fromRight [] $ runParser fileParser "" file

type Op = Int -> Int -> Int
(|||) :: Op
--a ||| b = read $ show a <> show b 
a ||| b = let bBase = floor $ logBase 10 $ fromIntegral b
              in a* 10^(bBase+1) +b

isSolution :: [Op] -> Problem  -> Bool
isSolution opList (target, nums) =  target `elem`  getViableResults nums where 
  getViableResults :: [Int] -> [Int]
  getViableResults [] = []
  getViableResults (x:xs) = foldl updateResults [x] xs where
    updateResults resultsSoFar a = filter (<= target) $ [(`op` a) | op <- opList ] <*> resultsSoFar 

sumOfSolvables :: [Problem] -> [Op] -> Int
sumOfSolvables problemList opList = sum . map fst  $ filter (isSolution opList)  problemList 

getSolutions7 :: String -> IO (Int, Int)
getSolutions7 filename = do 
  problemList <- parseFile <$> readFile filename
  let solution1 = sumOfSolvables problemList [(+), (*)]
      solution2 = sumOfSolvables problemList [(+), (*), (|||)]
  return (solution1, solution2)

--getViableCombos :: Problem -> [Int]
--getViableCombos (target, nums) = go $ reverse nums where
--  go :: [Int] -> [Int]
--  go [] = []
--  go [a] = [a]
--  go (a:rest) = filter (<= target) $ [(a+), (a*), (||| a )] <*> go rest

-- >>> parseTest lineParser testL
