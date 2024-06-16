module Day2.Run (run) where
import Lib (req)
import Data.Char (ord)

count :: ((Int, Int) -> Int) -> String -> Int
count pts = sum.map (pts.parse).lines

points :: (Int, Int) -> Int
points (a, b) = b + 3 * ((b - a + 1) `mod` 3)

points2 :: (Int, Int) -> Int
points2 (a, b) = 3 * b - 2 + ((a + b) `mod` 3)

parse :: String -> (Int, Int)
parse s = ((ord $ s !! 0) - 64, (ord $ s !! 2) - 87)

run :: IO ()
run = do
  r <- req 2
  putStrLn.show.count points $ r
  putStrLn.show.count points2 $ r