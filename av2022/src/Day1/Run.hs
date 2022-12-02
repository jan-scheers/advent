module Day1.Run (run) where
import Lib (req)
import Data.List.Split (splitWhen)
import Data.List (sort)

maxCal :: [[Int]] -> Int
maxCal = maximum.map sum

max3Cal :: [[Int]] -> Int
max3Cal = sum.take 3.reverse.sort.map sum

parse :: String -> [[Int]]
parse = (map.map) read.splitWhen (=="").lines

run :: IO ()
run = do
  r <- req 1
  putStrLn.show.maxCal.parse $ r
  putStrLn.show.max3Cal.parse $ r
  

