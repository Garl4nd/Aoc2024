module Main (
  main,
) where

import Control.Monad (unless, (>=>))
import InputDownloader (runFetchProblemDataToFiles)
import Lib
import System.TimeIt (timeItNamed)
import Text.Read (readMaybe)
import Useful (splitOn)
mainLoop :: IO ()
mainLoop = do
  putStrLn "Which problem do you want to solve?"
  prompt <- getLine
  unless (prompt `elem` ["e", "end"]) $ do
    let problemId = readMaybe prompt
    case problemId of
      Just day -> case maybeSolver day of
        Just solver -> do
          let inputFile = "inputs/" <> show day <> ".txt"
          runFetchProblemDataToFiles 2024 day inputFile ("descriptions/" <> show day <> ".html")
          res <- solver inputFile
          timeItNamed "The solution took " $ putStrLn $ "The solution of problem #" <> show day <> " is: " <> show res
        Nothing -> putStrLn "Not yet solved"
      Nothing -> putStrLn "Not a number" --       return True
    mainLoop

-- solver :: Int -> Maybe (IO (Int, Int))
maybeSolver ::  Int -> Maybe (String -> IO (Int,Int))
maybeSolver day = case day of
  --  solver <- maybeSolver
  -- Just $ do runFetchProblemDataToFiles 2024 day inputFile ("descriptions/" <> show day <> ".html") >> solver inputFile
  -- where
  -- maybeSolver = case day of
  1 -> Just getSolutions1
  2 -> Just getSolutions2
  3 -> Just getSolutions3
  4 -> Just getSolutions4
  5 -> Just getSolutions5
  6 -> Just getSolutions6
  7 -> Just getSolutions7
  8 -> Just getSolutions8 -- 9 -> Just $ getSolutions9 "inputs/9.txt"
  9 -> Just getSolutions9 -- 9 -> Just $ getSolutions9 "inputs/9.txt"
  10 -> Just getSolutions10
  11 -> Just getSolutions11
  12 -> Just getSolutions12
  13 -> Just getSolutions13
  14 -> Just $ getSolutions14
  15 -> Just $ getSolutions15
  16 -> Just $ getSolutions16
  17 -> Just $ getSolutions17
  18 -> Just $ \filename -> do 
    (a,b) <- getSolutions18  filename
    let [bx, by] = splitOn ',' b  
    return (a, read $ bx<>by)

  -- 172 -> Just $ getSolutions17Old "inputs/17.txt"
  -- 173 -> Just $ getSolutions17Astar "inputs/17.txt"
  -- 18 -> Just $ getSolutions18 "inputs/18.txt"
  -- 19 -> Just $ getSolutions19 "inputs/19.txt"
  -- 20 -> Just $ getSolutions20 "inputs/20.txt"
  -- 21 -> Just $ getSolutions21 "inputs/21.txt"
  -- 22 -> Just $ getSolutions22 "inputs/22.txt"
  -- 23 -> Just $ getSolutions23 "inputs/23.txt"
  -- 24 -> Just $ getSolutions24 "inputs/24.txt"
  -- 25 -> Just $ getSolutions25 "inputs/25.txt"
  _ -> Nothing

-- inputFile = "inputs/" <> show day <> ".txt"

main :: IO ()
main = do
  mainLoop
  putStrLn "Goodbye!"
