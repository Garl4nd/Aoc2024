module N10 (getSolutions10) 
where
import Control.Arrow
import Control.Monad ((>=>))
import Data.Function.Memoize (memoFix)
import Useful(strToCharGrid, GridPos)
import qualified Data.Array as A
import Data.Array ((!))
import Data.Char (digitToInt)
import Data.List (nub)

type NumGrid = A.Array GridPos Int 
type Hike = [GridPos]
type Memo f = f -> f

parseFile :: String -> NumGrid 
parseFile =  fmap digitToInt . strToCharGrid

neighbors :: GridPos -> [GridPos]
neighbors (y,x) = [(y+1,x), (y-1,x), (y, x-1), (y, x+1)]

(<&&>) = liftA2 (&&)

findAllHikesFrom :: NumGrid -> Memo (GridPos -> [Hike])
findAllHikesFrom  grid = go where 
  go  :: Memo (GridPos -> [Hike])
  go go pos 
    | valAt pos == 9 = [[pos]]  
    | otherwise = let  
      hikeableNeighbors = filter ( A.inRange bounds <&&> ((valAt pos +1 == ).valAt))  $  neighbors pos 
      -- joinHereWithPathsFrom = map (pos:).go 
      --concatMap joinHereWithPathsFrom hikeableNeighbors
      in [pos:path | paths <- go <$> hikeableNeighbors, path <-paths] 
  bounds = A.bounds grid
  valAt = (grid !)

findAllHikes :: NumGrid -> [[Hike]]
findAllHikes grid = findAllHikesFromM <$> filter ((== 0).(grid !))  (A.indices grid) where 
  findAllHikesFromM = memoFix (findAllHikesFrom grid)

finalPositionCount :: [Hike] -> Int
finalPositionCount  = length . nub . map last 

solution1 :: NumGrid -> Int
solution1 = sum . map  finalPositionCount . findAllHikes 

solution2 :: NumGrid -> Int
solution2 = sum . map  length . findAllHikes 

getSolutions10 :: String -> IO (Int, Int)
getSolutions10 = readFile >=> (parseFile >>> (solution1 &&& solution2) >>> return)