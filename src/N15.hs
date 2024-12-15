{-# LANGUAGE ScopedTypeVariables #-}

module N15 () where

import Control.Monad (foldM, guard, unless, when)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.State (State, get, put)
import Data.Array.Base (MArray (getBounds), STUArray, freezeSTUArray, modifyArray, readArray, thawSTUArray, writeArray)
import Data.Array.Unboxed (indices, (!))
import qualified Data.Array.Unboxed as A
import Data.List (find, findIndex, inits)
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Debugging (traceWInfo)
import Useful (CharGridU, GridPos, appendGridToFile, charGridToStr, saveGridToFile, splitBySubstr, strToCharGrid)

type STCharGrid s = STUArray s GridPos Char

-- data Direction = L | U | R | D deriving (Eq, Show)
type Direction = GridPos -> GridPos
charToDir :: Char -> Maybe Direction
charToDir '^' = Just (\(y, x) -> (y - 1, x))
charToDir 'v' = Just (\(y, x) -> (y + 1, x))
charToDir '<' = Just (\(y, x) -> (y, x - 1))
charToDir '>' = Just (\(y, x) -> (y, x + 1))
charToDir _ = Nothing
trace = traceWInfo False

type RobotMover s = ReaderT (STCharGrid s, STRef s GridPos) (ST s) ()

parseFile :: String -> (CharGridU, [Direction], GridPos)
parseFile file =
  let
    [gridStr, directions] = trace "splits" $ splitBySubstr "\n\n" file
    gridAr = strToCharGrid gridStr
    initPos = fromJust $ find (\pos -> gridAr ! pos == '@') $ indices gridAr
   in
    (gridAr, mapMaybe charToDir directions, initPos)

animateRobot :: [Direction] -> RobotMover s
animateRobot [] = return ()
animateRobot (currentInstruction : remDirections) = do
  moveRobot currentInstruction
  animateRobot remDirections

moveRobot :: forall s. Direction -> RobotMover s
moveRobot dir = do
  (ar, currentPosRef) <- ask
  currentPos <- lift $ readSTRef currentPosRef
  bounds <- lift $ getBounds ar
  let movePos = dir currentPos
  unless (A.inRange bounds movePos) $ return ()
  moveVal <- lift $ readArray ar movePos
  let moveCurrent = do
        writeArray ar currentPos '.'
        writeArray ar movePos '@'
        writeSTRef currentPosRef movePos
  let moveableList :: GridPos -> [GridPos] -> ST s (Maybe [GridPos])
      moveableList pos ls =
        if not $ A.inRange bounds pos
          then return Nothing
          else do
            val <- ar `readArray` pos
            case val of
              '#' -> return Nothing
              '.' -> return $ Just (pos : ls)
              'O' -> moveableList (dir pos) (pos : ls)

  -- _ -> return Nothing
  case moveVal of
    '#' -> return ()
    _ -> do
      maybeMoves <- lift $ moveableList movePos []
      case maybeMoves of
        Nothing -> return ()
        Just moves -> lift $ do
          mapM_ (\pos -> writeArray ar pos 'O') moves
          moveCurrent

evolveAr :: (CharGridU, [Direction], GridPos) -> CharGridU
evolveAr (ar, directions, initPos) = runST $ do
  star <- thawSTUArray ar
  pos <- newSTRef initPos
  runReaderT (animateRobot directions) (star, pos)
  freezeSTUArray star

gpsCoordinateSum :: CharGridU -> Int
gpsCoordinateSum charGrid =
  let coords = filter (\pos -> charGrid ! pos == 'O') $ A.indices charGrid
   in sum $ map (\(y, x) -> 100 * (y - 1) + x - 1) coords

solution1 = gpsCoordinateSum . evolveAr . parseFile

test :: String -> IO ()
test inputFile = do
  (ar, dirs, initPos) <- parseFile <$> readFile inputFile
  saveGridToFile "outputs/origAr.txt" ar
  let modFile = "outputs/modar.txt"
  writeFile modFile ""
  mapM_ (\dirs -> appendGridToFile modFile (evolveAr (ar, dirs, initPos)) >> appendFile modFile "\n\n") $ inits dirs
