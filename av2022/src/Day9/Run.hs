module Day9.Run (run) where
import Lib (req)

import Data.Set (Set)
import qualified Data.Set as Set

type Point = (Int, Int)

part1 :: String -> Int
part1 = length.fst.foldl command (Set.empty, replicate 2 (0, 0)).parse

part2 :: String -> Int
part2 = length.fst.foldl command (Set.empty, replicate 10 (0, 0)).parse

parse :: String -> [(Char, Int)]
parse = map (\s -> (s !! 0, read.drop 2 $ s)).lines 

command :: (Set Point, [Point]) -> (Char, Int) -> (Set Point, [Point])
command (visited, snake) (c, i) = let path = take i.drop 1.iterate (step dir) $ snake in
    (foldr (\p -> Set.insert (head p)) visited path, last path)
    where dir = (case c of
            'U' -> (1, 0)
            'D' -> (-1, 0)
            'L' -> (0, -1)
            _ -> (0, 1))

step :: (Int, Int) -> [Point] -> [Point]
step (dx, dy) snake = foldr (\sg sn@(tl:_) -> sg `follow` tl:sn) [hd] (init snake)
    where hd = (\(xh, yh) -> (xh+dx, yh+dy)).last $ snake

follow :: Point -> Point -> Point
follow  (xt, yt) (xn, yn) = let (dx, dy) = (xn - xt, yn - yt) in if dx ^ (2::Int) + dy ^ (2::Int) <= 2 
        then (xt, yt)
        else (xn - quot dx 2, yn - quot dy 2)

run :: IO ()
run = do
    putStrLn "--- Day 9 ---"
    r <- req 9
    putStrLn.((++) "part 1: ").show.part1 $ r
    putStrLn.((++) "part 2: ").show.part2 $ r