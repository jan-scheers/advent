module Day12.Run (run) where

import Lib (req)
import Data.Char (ord)
import Data.Vector (Vector, (!), (!?), (//))
import qualified Data.Vector as Vec
import Data.Maybe (mapMaybe)

type Matrix a = Vector (Vector a)
type Coord = (Int, Int)

parse :: String -> (Coord, Coord, Matrix Int)
parse s = (start, end, (fmap.fmap) (flip (-) $ ord 'a') $ hMap <# (start, ord 'a') <# (end, ord 'z'))
    where hMap = Vec.fromList.map (Vec.fromList.map ord).lines $ s
          start = find (== ord 'S') hMap
          end = find (== ord 'E') hMap

find :: (a -> Bool) -> Matrix a -> Coord
find f m = head $ mapMaybe (\i -> Vec.findIndex f (m ! i) >>= \j -> Just (i, j)) [0..length m]

(<#) :: Matrix a -> (Coord, a) -> Matrix a
(<#) mat ((i,j), a) = mat // [(i, (mat ! i) // [(j, a)])]

(#) :: Matrix a -> Coord -> a
(#) mat (i, j) = mat ! i ! j


bfs :: Matrix Int -> [Coord] -> Matrix Int -> Matrix Int
bfs _ [] vert = vert
bfs heightmap (curr:queue) vert = bfs heightmap (queue ++ es) (
    foldr (\edge -> flip (<#) (edge, d+1)) vert es)
    where es = edges heightmap vert curr
          d = vert # curr

edges :: Matrix Int -> Matrix Int -> Coord -> [Coord]
edges mat vert curr@(i, j) = mapMaybe (\(dx, dy) -> 
    let next@(ni, nj) = (i+dx, j+dy) in mat !? ni >>= (!? nj) >>= \nh -> 
            if nh >= h - 1 && vert # next > d + 1 then Just next else Nothing)
    [(-1,0),(0,-1),(0,1),(1,0)]
    where h = mat # curr
          d = vert # curr

trailHead :: (Int, Int) -> Coord -> Matrix Int
trailHead (m, n) s = Vec.replicate m (Vec.replicate n maxBound) <# (s, 0)


part1 :: String -> Int
part1 r = (# s).bfs mat [e] $ trailHead (m, n) e
    where (s, e, mat) = parse r
          (m, n) = (length mat, length $ mat ! 0)

part2 :: String -> Int
part2 r = minimum.(\vert -> map ((#) vert) zeros).bfs mat [e] $ trailHead (m, n) e
    where (_, e, mat) = parse r
          (m, n) = (length mat, length $ mat ! 0)
          zeros = Vec.ifoldr (\i row -> (++) $ Vec.ifoldr (\j v zs -> if v == 0 then (i, j):zs else zs) [] row) [] mat
           

run :: IO ()
run = do
    putStrLn "--- Day 12 ---"
    r <- req 12
    putStrLn.((++) "part 1: ").show.part1 $ r
    putStrLn.((++) "part 2: ").show.part2 $ r