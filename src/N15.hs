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

data Direction = L | U | R | D deriving (Eq, Show)

-- type Direction = GridPos -> GridPos
charToDir :: Char -> Maybe Direction
charToDir '^' = Just U
charToDir 'v' = Just D
charToDir '<' = Just L
charToDir '>' = Just R
charToDir _ = Nothing

left (y, x) = (y, x - 1)
right (y, x) = (y, x + 1)
up (y, x) = (y - 1, x)
down (y, x) = (y + 1, x)

moveDir :: Direction -> GridPos -> GridPos
moveDir U (y, x) = (y - 1, x)
moveDir D (y, x) = (y + 1, x)
moveDir L (y, x) = (y, x - 1)
moveDir R (y, x) = (y, x + 1)

trace :: (Show a) => String -> a -> a
trace = traceWInfo False

type RobotMover s = ReaderT (STCharGrid s, STRef s GridPos) (ST s) ()

parseFile :: String -> (CharGridU, [Direction], GridPos)
parseFile file =
  let
    [gridStr, directions] = trace "splits" $ splitBySubstr "\n\n" (trace "f" file)
    gridAr = strToCharGrid gridStr
    initPos = fromJust $ find (\pos -> gridAr ! pos == '@') $ indices gridAr
   in
    (gridAr, mapMaybe charToDir directions, initPos)

animateRobot :: [Direction] -> RobotMover s
animateRobot [] = return ()
animateRobot (currentInstruction : remDirections) = do
  moveRobot2 currentInstruction
  animateRobot remDirections

moveRobot :: forall s. Direction -> RobotMover s
moveRobot d = do
  let dir = moveDir d
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

duplicateInput :: String -> String
duplicateInput [] = []
duplicateInput (c : rest)
  | c == '.' = '.' : '.' : duplicateInput rest
  | c == '#' = '#' : '#' : duplicateInput rest
  | c == 'O' = '[' : ']' : duplicateInput rest
  | c == '@' = '@' : '.' : duplicateInput rest
  | otherwise = c : duplicateInput rest
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

gpsCoordinateSum2 :: CharGridU -> Int
gpsCoordinateSum2 charGrid =
  let coords = filter (\pos -> charGrid ! pos == '[') $ A.indices charGrid
   in sum $ map (\(y, x) -> 100 * (y - 1) + x - 1) coords

solution1 = gpsCoordinateSum . evolveAr . parseFile
solution2 = gpsCoordinateSum2 . evolveAr . parseFile . duplicateInput

test :: String -> IO ()
test inputFile = do
  (ar, dirs, initPos) <- parseFile <$> readFile inputFile
  saveGridToFile "outputs/origAr.txt" ar
  let modFile = "outputs/modar.txt"
  writeFile modFile ""
  mapM_ (\dirs -> appendGridToFile modFile (evolveAr (ar, dirs, initPos)) >> appendFile modFile "\n\n") $ inits dirs

test2 :: String -> IO ()
test2 inputFile = do
  (ar, dirs, initPos) <- (parseFile . duplicateInput) <$> readFile inputFile
  saveGridToFile "outputs/origAr2.txt" ar
  let modFile = "outputs/modar2.txt"
  writeFile modFile ""
  mapM_ (\dirs -> appendGridToFile modFile (evolveAr (ar, dirs, initPos)) >> appendFile modFile (("\n" <> if null dirs then "" else (show $ last dirs)) <> "\n")) $ inits dirs

moveRobot2 :: forall s. Direction -> RobotMover s
moveRobot2 d = do
  let dir = moveDir d
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
  let moveableList :: GridPos -> ST s (Maybe [GridPos])
      moveableList pos =
        if not $ A.inRange bounds pos
          then return Nothing
          else do
            val <- ar `readArray` (trace "pos" pos)
            case trace "val" val of
              '#' -> return Nothing
              '.' -> return $ Just []
              _ ->
                if
                  | d `elem` [L, R] -> do
                      let rightPos = dir pos
                      maybeLs <- moveableList (dir rightPos)
                      case maybeLs of
                        Just ls -> return $ Just (pos : rightPos : ls)
                        _ -> return Nothing
                  | otherwise -> do
                      let otherPos = if val == '[' then right pos else left pos

                      maybeLs1 <- moveableList (dir pos)
                      maybeLs2 <- moveableList (dir $ otherPos)
                      case (maybeLs1, maybeLs2) of
                        (Just ls1, Just ls2) -> return $ Just (pos : otherPos : ls1 ++ ls2)
                        _ -> return Nothing

  -- _ -> return Nothing
  case moveVal of
    '#' -> return ()
    _ -> do
      maybeMoves <- lift $ moveableList movePos
      case maybeMoves of
        Nothing -> return ()
        Just moves -> lift $ do
          vals <- mapM (\pos -> readArray ar pos) moves
          mapM_ (\pos -> writeArray ar pos '.') $ reverse $ trace "moves" moves
          mapM_ (\(pos, val) -> writeArray ar (dir pos) val) $ zip moves vals
          moveCurrent
