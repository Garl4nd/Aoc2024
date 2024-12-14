{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module N14 (getSolutions14) where

import Control.Arrow
import Control.Monad ((>=>))
import qualified Data.Array as A
import Data.Either (fromRight)
import Data.List (intercalate)
import Data.Maybe (isNothing)
import Data.Tuple (swap)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import Useful (charGridToStr, groupByUnique)

type SParser = Parsec Void String
type Position = (Int, Int)
type Velocity = (Int, Int)
data Robot = Robot {pos :: Position, vel :: Velocity} deriving (Show)

-- p=78,41 v=-61,-6
robotParser :: SParser Robot
robotParser = do
  pos <- string "p=" *> ((,) <$> (L.decimal <* char ',') <*> (L.decimal <* char ' '))
  vel <- string "v=" *> ((,) <$> (signedParser <* char ',') <*> signedParser)
  return $ Robot{..}
 where
  signedParser :: SParser Int
  signedParser = do
    sign <- try . optional $ char '-'
    num <- L.decimal
    if isNothing sign then return num else return $ negate num

mod2 :: Position -> Position -> Position
mod2 (x, y) (m, n) = (x `mod` m, y `mod` n)

(x, y) |+| (u, v) = (x + u, y + v)
(|*|) s (x, y) = (s * x, s * y)
moveRobot :: Position -> Int -> Robot -> Robot
moveRobot dims steps robot@Robot{pos, vel} =
  let
    pos' = pos |+| (steps |*| vel)
   in
    robot{pos = mod2 pos' dims}

data Quadrant = TL | TR | BL | BR | Center deriving (Eq, Show, Ord)
classifyRobot :: Position -> Robot -> Quadrant
classifyRobot (w, h) Robot{pos = (x, y)}
  | x < w `div` 2 && y < h `div` 2 = TL
  | x < w `div` 2 && y > h `div` 2 = BL
  | x > w `div` 2 && y < h `div` 2 = TR
  | x > w `div` 2 && y > h `div` 2 = BR
  | otherwise = Center

dims :: (Int, Int) = (101, 103)
solution1 :: [Robot] -> Int
solution1 robots =
  let
    movedRobots = moveRobot dims 100 <$> robots
    quadrGroups = groupByUnique id $ classifyRobot dims <$> movedRobots
   in
    product [num | (quadr, num) <- quadrGroups, quadr /= Center]

printRobots :: [Robot] -> String
printRobots robots =
  let
    strAr = A.accumArray (const id) '.' ((0, 0), swap dims |+| (-1, -1)) [(swap $ pos robot, 'x') | robot <- robots]
   in
    intercalate "\n" $ take 50 $ drop 25 $ charGridToStr strAr

generateImage :: String -> [Robot] -> IO ()
generateImage filename robots = do
  let str = printRobots robots
  appendFile filename str
  appendFile filename "\n\n"

generateImages :: String -> [Robot] -> Int -> Int -> IO ()
generateImages filename robot from to = do
  writeFile filename ""
  let robotSeq = drop from $ take to (iterate (map $ moveRobot dims 1) robot)
  mapM_ (generateImage filename) robotSeq

solution2 :: [Robot] -> Int
solution2 = humanSolver
 where
  humanSolver = const 8270

parseFile :: String -> [Robot]
parseFile file = fromRight [] $ runParser (sepEndBy robotParser newline) "" file

getSolutions14 :: String -> IO (Int, Int)
getSolutions14 = readFile >=> (parseFile >>> (solution1 &&& solution2) >>> return)
