module N12 (getSolutions12)
where
import Control.Arrow
import Control.Monad ((>=>))
import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.Map as M
import  Data.Array ((!))

import Useful (strToCharGrid, CharGrid, GridPos)
type PositionSet = S.Set GridPos

type Crop = Char

parseFile :: String -> CharGrid
parseFile = strToCharGrid

neighbors :: GridPos -> [GridPos]
neighbors (y,x) = [(y+1,x), (y-1,x), (y, x-1), (y, x+1)]

(<&&>) = liftA2 (&&)
growRegion :: CharGrid -> GridPos -> (Crop, PositionSet)
growRegion charGrid startPos = (val, go S.empty $ S.singleton startPos) where 
  go :: PositionSet -> PositionSet -> PositionSet 
  go currentSet boundary
    | S.null boundary = currentSet
    | otherwise = let newSet = S.union currentSet boundary
                      newBoundary = S.unions $ S.map (S.fromList . filter ((`S.notMember` currentSet) <&&> inBounds <&&> isSameCrop) . neighbors ) boundary
                  in go newSet newBoundary
  val = charGrid ! startPos 
  inBounds = A.inRange bounds 
  isSameCrop pos = charGrid ! pos == val
  bounds = A.bounds charGrid
  


getSolutions12 :: String -> IO (Int, Int)
getSolutions12 = const $ return (0,0)-- readFile >=> (parseFile >>> (solution1 &&& solution2) >>> return)