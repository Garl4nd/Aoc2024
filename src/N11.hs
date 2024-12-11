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
  | let stoneStr = show stone, let sl = length stoneStr, even sl =  
      let (leftNum, rightNum) = splitAt (sl `div` 2) stoneStr
          leftResult  = blink (n-1) (read leftNum) 
          rightResult = blink (n-1) (read rightNum)  
        in leftResult + rightResult
  | otherwise = blink (n-1) $  stone * 2024

multiStoneBlink :: Int -> [Stone] -> Int
multiStoneBlink blinkCount = sum . map (blinkM2 blinkCount) where 
     blinkM2 = memoFix2 blink

parseFile :: String -> [Stone]
parseFile = map read . words


getSolutions11 :: String -> IO (Int, Int)
getSolutions11 = readFile >=> (parseFile >>>  (multiStoneBlink 25 &&& multiStoneBlink 75) >>> return)