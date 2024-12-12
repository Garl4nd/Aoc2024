module N11 (getSolutions11)
where
import Control.Arrow
import Control.Monad ((>=>))
import Data.Function.Memoize (memoFix2)

type Memo f = f -> f
type Stone = Int

blink :: Memo (Int -> Stone -> Int)
blink _ 0 _ =  1
blink blink n stone 
  | stone == 0 = blink (n-1) 1
  | stoneStr <- show stone, sl <- length stoneStr, even sl =  
      let (leftNum, rightNum) = splitAt (sl `div` 2) stoneStr
          leftResult  = blink (n-1) (read leftNum) 
          rightResult = blink (n-1) (read rightNum)  
        in leftResult + rightResult
  | otherwise = blink (n-1) $  stone * 2024

multiStoneBlink :: Int -> [Stone] -> Int
multiStoneBlink blinkCount = sum . map (blinkMemo blinkCount) where 
     blinkMemo = memoFix2 blink
     --blinkNonMemo = fix blink

parseFile :: String -> [Stone]
parseFile = map read . words

solution1 = multiStoneBlink 25
solution2 = multiStoneBlink 75

getSolutions11 :: String -> IO (Int, Int)
getSolutions11 = readFile >=> (parseFile >>> (solution1 &&& solution2) >>> return)