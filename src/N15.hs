{-# LANGUAGE ScopedTypeVariables #-}

module N15 (getSolutions15) where

import Control.Monad (unless)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Array.Base (MArray (getBounds), STUArray, freezeSTUArray, modifyArray, readArray, thawSTUArray, writeArray)
import Data.Array.Unboxed ((!))
import qualified Data.Array.Unboxed as A
import Data.List (find, findIndex, inits)
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Useful (CharGridU, GridPos, appendGridToFile, charGridToStr, saveGridToFile, splitBySubstr, strToCharGrid)

type STCharGrid s = STUArray s GridPos Char
type RobotMover s = ReaderT (STCharGrid s, STRef s GridPos) (ST s)
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
moveDir U = up
moveDir D = down
moveDir L = left
moveDir R = right

parseFile :: String -> (CharGridU, [Direction], GridPos)
parseFile file =
  let
    [gridStr, directions] = splitBySubstr "\n\n" file
    gridAr = strToCharGrid gridStr
    initPos = fromJust $ find (\pos -> gridAr ! pos == '@') $ A.indices gridAr
   in
    (gridAr, mapMaybe charToDir directions, initPos)

animateRobot :: [Direction] -> RobotMover s ()
animateRobot [] = return ()
animateRobot (currentInstruction : remDirections) = do
  moveRobotAndCrates currentInstruction
  animateRobot remDirections

moveRobotAndCrates :: forall s. Direction -> RobotMover s ()
moveRobotAndCrates dir = do
  let move = moveDir dir
  (ar, currentPosRef) <- ask
  currentPos <- lift $ readSTRef currentPosRef
  let movePos = move currentPos
  bounds <- lift $ getBounds ar
  unless (A.inRange bounds movePos) $ return ()
  moveVal <- lift $ readArray ar movePos
  case moveVal of
    '#' -> return ()
    _ -> do
      maybeMoves <- moveableCrates movePos dir
      case maybeMoves of
        Nothing -> return ()
        Just moves -> do
          lift $ do
            vals <- mapM (readArray ar) moves
            mapM_ (\pos -> writeArray ar pos '.') $ reverse moves
            mapM_ (\(pos, val) -> writeArray ar (move pos) val) $ zip moves vals
          moveRobot movePos

moveableCrates :: GridPos -> Direction -> RobotMover s (Maybe [GridPos])
moveableCrates pos dir = do
  (ar, _) <- ask
  bounds <- lift $ getBounds ar
  let move = moveDir dir
  if not $ A.inRange bounds pos
    then return Nothing
    else do
      val <- lift $ ar `readArray` pos
      case val of
        '#' -> return Nothing
        '.' -> return $ Just []
        'O' -> do
          maybeLs <- moveableCrates (move pos) dir
          case maybeLs of
            Just ls -> return $ Just (pos : ls)
            _ -> return Nothing
        _ ->
          if dir `elem` [L, R]
            then do
              let rightPos = move pos
              maybeLs <- moveableCrates (move rightPos) dir
              case maybeLs of
                Just ls -> return $ Just (pos : rightPos : ls)
                _ -> return Nothing
            else do
              let otherPos = if val == '[' then right pos else left pos

              maybeLs1 <- moveableCrates (move pos) dir
              maybeLs2 <- moveableCrates (move otherPos) dir
              case (maybeLs1, maybeLs2) of
                (Just ls1, Just ls2) -> return $ Just (pos : otherPos : ls1 ++ ls2)
                _ -> return Nothing

moveRobot :: GridPos -> RobotMover s ()
moveRobot movePos = do
  (ar, currentPosRef) <- ask
  lift $ do
    currentPos <- readSTRef currentPosRef
    writeArray ar currentPos '.'
    writeArray ar movePos '@'
    writeSTRef currentPosRef movePos

complicateInput :: String -> String
complicateInput [] = []
complicateInput (c : rest)
  | c == '.' = ".." ++ complicateInput rest
  | c == '#' = "##" ++ complicateInput rest
  | c == 'O' = "[]" ++ complicateInput rest
  | c == '@' = "@." ++ complicateInput rest
  | otherwise = c : complicateInput rest
evolveAr :: (CharGridU, [Direction], GridPos) -> CharGridU
evolveAr (ar, directions, initPos) = runST $ do
  star <- thawSTUArray ar
  pos <- newSTRef initPos
  runReaderT (animateRobot directions) (star, pos)
  freezeSTUArray star

gpsCoordinateSum :: Char -> CharGridU -> Int
gpsCoordinateSum symbol charGrid =
  let coords = filter (\pos -> charGrid ! pos == symbol) $ A.indices charGrid
   in sum $ map (\(y, x) -> 100 * (y - 1) + x - 1) coords
solution1 = gpsCoordinateSum 'O' . evolveAr
solution2 = gpsCoordinateSum '[' . evolveAr

getSolutions15 :: String -> IO (Int, Int)
getSolutions15 filename =
  do
    input <- readFile filename
    let complicatedInput = complicateInput input
    return (solution1 . parseFile $ input, solution2 . parseFile $ complicatedInput)

test :: String -> IO ()
test inputFile = do
  (ar, dirs, initPos) <- parseFile <$> readFile inputFile
  saveGridToFile "outputs/origAr.txt" ar
  let modFile = "outputs/modar.txt"
  writeFile modFile ""
  mapM_ (\dirs -> appendGridToFile modFile (evolveAr (ar, dirs, initPos)) >> appendFile modFile "\n\n") $ inits dirs

test2 :: String -> IO ()
test2 inputFile = do
  (ar, dirs, initPos) <- (parseFile . complicateInput) <$> readFile inputFile
  saveGridToFile "outputs/origAr2.txt" ar
  let modFile = "outputs/modar2.txt"
  writeFile modFile ""
  mapM_ (\dirs -> appendGridToFile modFile (evolveAr (ar, dirs, initPos)) >> appendFile modFile (("\n" <> if null dirs then "" else (show $ last dirs)) <> "\n")) $ inits dirs
