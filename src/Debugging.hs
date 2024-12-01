module Debugging (traceWInfo, traceWInfo2) where

import Debug.Trace

traceWInfo :: (Show a) => Bool -> [Char] -> a -> a
traceWInfo active infoStr x =
  if active
    then trace ("\n** " ++ infoStr ++ " " ++ show x ++ "**\n") x
    else x

traceWInfo2 :: (Show a) => Bool -> [Char] -> a -> b -> b
traceWInfo2 active infoStr x passThrough =
  if active
    then
      trace ("\n** " ++ infoStr ++ " " ++ show x ++ "**\n") passThrough
    else passThrough
