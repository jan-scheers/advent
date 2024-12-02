module Year22.Day14.Run (run) where

import Year22.Lib (req)
import Data.Matrix ( safeGet, setElem, zero, Matrix, (!), nrows, ncols )
import Data.List ( unfoldr ) 
import Data.List.Split ( splitOn )

type Point = (Int, Int)
type Line = [Point]

parse :: String -> [Line]
parse = map (map ((\is -> (is !! 1, is !! 0)).map read.splitOn ",").splitOn " -> ").lines

drawLine :: Line -> Matrix Int -> Matrix Int
drawLine ln = flip (foldr segment) $ zip (init ln) (tail ln)

segment :: (Point, Point) -> Matrix Int -> Matrix Int
segment ((a, b), (c, d)) = flip (foldr (setElem 1)) [(a + i*dx, b + i*dy) | i <- [0..len]]
    where len = abs (c - a) + abs (d - b)
          (dx, dy) = ((c - a) `quot` len, (d - b) `quot` len)

sand :: (Int, Int) -> Matrix Int -> Maybe (Matrix Int)
sand (i, j) mat = safeGet (i + 1) j mat >>= \a -> case a of
    0 -> sand (i+1, j) mat
    _ -> safeGet (i + 1) (j - 1) mat >>= \b -> case b of
        0 -> sand (i + 1, j - 1) mat
        _ -> safeGet (i + 1) (j + 1) mat >>= \c -> case c of
            0 -> sand (i + 1, j + 1) mat
            _ -> if mat ! (i, j) == 0
                then Just (setElem 1 (i, j) mat) 
                else Nothing

normalise :: [Line] -> (Matrix Int, [Line])
normalise ps = (zero (m + 1) (2*m + 1), norm)
    where m = maximum.map (maximum.map fst) $ ps
          norm = (map.map) (\(a, b) -> (a + 1, b - 500 + m + 1)) ps

part1 :: [Char] -> [Matrix Int]
part1 s = unfoldr (\m -> sand drp m >>= \n -> Just (n, n)) start
    where (zmat, norm) = normalise.parse $ s
          start = foldr drawLine zmat norm
          drp = (1, ncols zmat `div` 2 + 1)

part2 :: [Char] -> [Matrix Int]
part2 s = unfoldr (\m -> sand drp m >>= \n -> Just (n, n)) start
    where (zmat, norm) = normalise.parse $ s
          start = foldr drawLine zmat ([(nrows zmat, 1), (nrows zmat, ncols zmat)]:norm)
          drp = (1, ncols zmat `div` 2 + 1)

run :: IO ()
run = do
    putStrLn "--- Day 14 ---"
    r <- req 14
    let p1 = part1 r
    putStrLn.((++) "part 1: ").show.length $ p1
    putStrLn.show.last $ p1
    let p2 = part2 r
    putStrLn.((++) "part 2: ").show.length $ p2
    putStrLn.show.last $ p2