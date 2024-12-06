module Download (download) where

import InputDownloader (runFetchProblemDataToFiles)
import System.Environment (getArgs)

download :: IO ()
download = do
  putStrLn "Hello from download"
  args <- getArgs
  case args of
    [day] ->
      let inputFileName = "inputs/" <> day <> ".txt"
          descFileName = "descriptions/" <> day <> ".html"
       in runFetchProblemDataToFiles 2024 (read day) inputFileName descFileName
    _ -> putStrLn "Need one argument!"
