module Year22.Day4.Run (run) where
import Year22.Lib (req)
import Data.List.Split (splitOn)

parse :: String -> [Int]
parse = (map read) . (concatMap $ splitOn "-").splitOn ","

eval :: [Int] -> Int
eval [a, b, c, d] = if a <= c && d <= b || c <= a && b <= d then 1 else 0
eval _ = 0

eval2 :: [Int] -> Int
eval2 is@[a, b, c, d] = if eval is == 1 || a <= c && c <= b || a <= d && d <= b then 1 else 0
eval2 _ = 0

calc :: String -> Int
calc = sum.map (eval.parse).lines

calc2 :: String -> Int
calc2 = sum.map (eval2.parse).lines

run :: IO ()
run = do
    r <- req 4
    putStrLn.show.calc $ r
    putStrLn.show.calc2 $ r