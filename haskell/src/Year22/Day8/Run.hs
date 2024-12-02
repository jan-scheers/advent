module Day8.Run (run) where
import Lib (file, req)
import Data.Char (digitToInt)

import Data.Vector (Vector, (!))
import qualified Data.Vector as Vec

type Matrix a = Vector (Vector a)

parse :: String -> Matrix (Int, Bool)
parse = Vec.fromList . map (Vec.fromList . map ((\d -> (d, False)).digitToInt)).lines

comb :: Matrix (Int, Bool) -> Matrix (Int, Bool)
comb mat = Vec.fromList.snd.Vec.foldr visible (Vec.replicate n (-1), []) $ mat
    where n = Vec.length mat
          visible row (mxs, rows) = (
            Vec.zipWith (\a (b, _) -> max a b) mxs row,
            Vec.imap (\j (h, v) -> (h, v || h > mxs ! j)) row : rows)

rot :: Matrix a -> Matrix a
rot mat = Vec.imap (\i -> Vec.imap (\j _ -> mat ! j ! (n - i - 1))) mat
    where n = Vec.length mat

part1 :: String -> Int
part1 = count.(!!4).iterate (rot.comb).parse
    where count = Vec.foldl (\a -> (+ a).(Vec.foldl (\b (_, v) -> b + if v then 1 else 0) 0)) 0

parse2 :: String -> Matrix Int
parse2 = Vec.fromList . map (Vec.fromList . map digitToInt).lines

checkHut :: Matrix Int -> (Int, Int) -> Int
checkHut mat (i, j) = product $ map (look mat (i, j)) [(-1,0),(0,-1),(0,1),(1,0)]

look :: Matrix Int -> (Int, Int) -> (Int, Int) -> Int
look mat (i, j) (dx, dy) = case span (\(a, b) -> mat ! a ! b < mat ! i ! j) treeLine of
    (ts, []) -> length ts
    (ts, _) -> length ts + 1
    where n = Vec.length mat
          treeLine = takeWhile (\(a, b) -> 0 <= a && a < n && 0 <= b && b < n) [(i+k*dx, j+k*dy) | k <- [1..]]

part2 :: String -> Int
part2 s = maximum $ map (checkHut mat) [(i,j) | i <- [0..n-1], j <- [0..n-1]]
    where mat = parse2 s
          n = length mat

run :: IO ()
run = do
    putStrLn "--- Day 8 ---"
    file 8 >>= putStrLn.((++) "part 1: ").show.part1
    file 8 >>= putStrLn.((++) "part 2: ").show.part2
    req 8 >>= putStrLn.((++) "part 1: ").show.part1
    req 8 >>= putStrLn.((++) "part 2: ").show.part2