module Download (download) where

import Lib (runFetchInputToFile)
import System.Environment (getArgs)

download :: IO ()
download = do
  print "Hello from download"
  args <- getArgs
  case args of
    [day] ->
      let fileName = "inputs/" <> day <> ".txt"
       in runFetchInputToFile 2024 (read day) fileName
    _ -> print "Need one argument!"
