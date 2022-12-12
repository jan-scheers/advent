module Day12.Run (run) where

import Lib (file)
import Data.Char (ord)
import Data.Vector (Vector, (!), (!?), (//))
import qualified Data.Vector as Vec
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

type Matrix a = Vector (Vector a)
type Coord = (Int, Int)
type Vert = (Int, (Int, Int))

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


dijkstra :: Matrix Int -> (Set Vert, Matrix Vert) -> Matrix Vert
dijkstra heightmap (prio, vert) = case Set.minView prio of
    Nothing -> vert
    Just ((d, curr), queue) -> if fst (vert # curr) /= d 
        then dijkstra heightmap (queue, vert)
        else dijkstra heightmap $ foldr 
            (\next prev@(q, vrt) -> if d + 1 >= fst (vert # next) then prev 
                else (Set.insert (d+1, next) q, vrt <# (next, (d+1, curr))))
            (queue, vert)
            (heightmap `edges` curr)

edges :: Matrix Int -> Coord -> [Coord]
edges mat (i, j) = mapMaybe (\(dx, dy) -> 
    let (ni, nj) = (i+dx, j+dy) in mat !? ni >>= (!? nj) >>= \v -> 
            if v >= h - 1 then Just (ni, nj) else Nothing) 
    [(-1,0),(0,-1),(0,1),(1,0)]
    where h = mat # (i, j)

trailHead :: (Int, Int) -> Coord -> (Set Vert, Matrix Vert)
trailHead (m, n) s = (Set.singleton (0, s), Vec.replicate m (Vec.replicate n (maxBound, (-1, -1)) ) <# (s, (0, s)))


part1 :: String -> Int
part1 r = fst.(# s).dijkstra mat $ trailHead (m, n) e
    where (s, e, mat) = parse r
          (m, n) = (length mat, length $ mat ! 0)

part2 :: String -> Int
part2 r = minimum.(\vert -> map (fst.(#) vert) zeros).dijkstra mat $ trailHead (m, n) e
    where (_, e, mat) = parse r
          (m, n) = (length mat, length $ mat ! 0)
          zeros = Vec.ifoldr (\i row -> (++) $ Vec.ifoldr (\j v zs -> if v == 0 then (i, j):zs else zs) [] row) [] mat
           

run :: IO ()
run = do
    putStrLn "--- Day 12 ---"
    r <- file 12
    putStrLn.((++) "part 1: ").show.part1 $ r
    putStrLn.((++) "part 2: ").show.part2 $ r