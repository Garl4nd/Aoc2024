module N7 (getSolutions7) where
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Data.Either (fromRight)
import Data.List (foldl')
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

(|||) :: Int -> Int -> Int
a ||| b = read $ show a <> show b 

type Op = Int -> Int -> Int
isSolution :: [Op] -> Problem  -> Bool
isSolution opList (target, nums) =  target `elem` getViableResults nums where 
  getViableResults :: [Int] -> [Int]
  getViableResults [] = []
  getViableResults (fstNum:restNums) = foldl updateResults [fstNum]  restNums where
    updateResults resultsSoFar a = filter (<= target) $ [(`op` a) | op <- opList ] <*> filter (<= target) resultsSoFar 

solution :: String -> [Op] -> Int
solution file opList = let 
  problemList = parseFile file
  in sum . map fst  $ filter (isSolution opList)  problemList 

getSolutions7 :: String -> IO (Int, Int)
getSolutions7 filename = do 
  file <- readFile filename
  let solution1 = solution file [(+), (*)]
      solution2 = solution file [(+), (*), (|||)]
  return (solution1, solution2)

--getViableCombos :: Problem -> [Int]
--getViableCombos (target, nums) = go $ reverse nums where
--  go :: [Int] -> [Int]
--  go [] = []
--  go [a] = [a]
--  go (a:rest) = filter (<= target) $ [(a+), (a*), (||| a )] <*> go rest

-- >>> parseTest lineParser testL
