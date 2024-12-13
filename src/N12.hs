module N12 (getSolutions12) where
import Control.Arrow
import Control.Monad ((>=>))
import qualified Data.Array as A
import qualified Data.Set as S
import Data.Set (member, notMember)
import  Data.Array ((!))

import Useful (strToCharGrid, CharGrid, GridPos, countIf)
import Data.Foldable (Foldable(toList))
type PositionSet = S.Set GridPos

parseFile :: String -> CharGrid
parseFile = strToCharGrid

neighbors :: GridPos -> [GridPos]
neighbors (y,x) = [(y,x-1), (y-1,x), (y, x+1), (y+1, x)]

getRegion :: CharGrid -> GridPos -> PositionSet
getRegion charGrid startPos = grow S.empty (S.singleton startPos) where 
  grow :: PositionSet -> PositionSet -> PositionSet 
  grow currentRegion boundary
    | S.null boundary = currentRegion
    | otherwise = let grownRegion = S.union currentRegion boundary
                      newBoundary = S.unions $ S.map (S.fromList . filter ((`S.notMember` currentRegion) <&&> inBounds <&&> isSameCrop) . neighbors ) boundary
                  in grow grownRegion newBoundary
  val = charGrid ! startPos 
  inBounds = A.inRange $ A.bounds charGrid 
  isSameCrop pos = charGrid ! pos == val
  (<&&>) = liftA2 (&&)

getAllRegions :: CharGrid ->  [PositionSet]
getAllRegions charGrid = go [] $ S.fromList  (A.indices charGrid) where
  go :: [PositionSet] -> PositionSet -> [PositionSet]
  go foundRegions unassignedSet
    | S.null unassignedSet = foundRegions
    | otherwise = let newRegion = getRegion charGrid (S.elemAt 0 unassignedSet) 
                      newUnassignedSet = S.difference unassignedSet newRegion in
                        go (newRegion : foundRegions) newUnassignedSet 

perimeter :: PositionSet ->  Int 
perimeter posSet = sum $  countIf (`S.notMember` posSet) . neighbors  <$> toList posSet

numOfSides :: PositionSet -> Int
numOfSides region = sum $ numCorners <$>  toList region where
 numCorners  (y,x) = countIf  
  (\(adj1, adj2, corner) -> 
  all (`S.notMember` region) [adj1, adj2] || all (`S.member` region) [adj1,adj2] && (corner `notMember` region))   touching8Neighbors  where
  touching8Neighbors = [((y+dy, x), (y, x+dx), (y+dy, x+dx)) | dy <- [-1, 1], dx <- [-1,1]]
solution1 :: CharGrid -> Int 
solution1 charGrid = sum $ liftA2 (*)  length perimeter  <$> getAllRegions charGrid   

solution2 :: CharGrid -> Int 
solution2 charGrid = sum $ liftA2 (*) length numOfSides  <$> getAllRegions charGrid   

getSolutions12 :: String -> IO (Int, Int)
getSolutions12 = readFile >=> (parseFile >>> (solution1 &&& solution2) >>> return)
