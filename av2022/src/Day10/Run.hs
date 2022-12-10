module Day10.Run (run) where
import Lib (file)
import Data.List.Split (chunksOf)

part1 :: String -> Int
part1 s = sum $ map (\i -> i * (exe s !! i)) [20, 60 .. 220]

part2 :: String -> String
part2 s = fmt $ map (\i -> let (p, x) = (exe s !! (i+1), i `rem` 40) in p - 1 <= x && x <= p + 1) [0..239]

fmt :: [Bool] -> String
fmt = unlines.chunksOf 40.map (\b -> if b then '\x2588' else ' ')

exe :: String -> [Int]
exe = reverse.foldl tick [1, 1].lines
    where tick xs@(x:_) cmd = case take 4 cmd of
            "noop" -> x:xs
            _ -> let i = read (drop 5 cmd) in (i+x):x:xs
          tick _ _ = []

run :: IO ()
run = do
    putStrLn "--- Day 10 ---"
    r <- file 10
    putStrLn.((++) "part 1: ").show.part1 $ r
    putStrLn.((++) "part 2:\n").part2 $ r