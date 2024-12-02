module Year22.Day3.Run (run) where
import Year22.Lib (req) 
import Data.Char (ord)
import Data.List (intersect)
import Data.List.Split (chunksOf)

parse :: String -> Int
parse = sum.map (head.uncurry intersect.half.map prio).lines

parse2 :: String -> Int
parse2 = sum.map (head.isect3).chunksOf 3.map (map prio).lines

isect3 :: [[Int]] -> [Int]
isect3 [a, b, c] = a `intersect` b `intersect` c
isect3 _ = [0]

prio :: Char -> Int
prio = (\o -> if o >= 97 then o - 96 else o - 38).ord

half :: [a] -> ([a], [a])
half xs = splitAt (length xs `div` 2) xs

run :: IO ()
run = do
  r <- req 3
  putStrLn.show.parse $ r 
  putStrLn.show.parse2 $ r 