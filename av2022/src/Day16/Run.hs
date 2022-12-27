module Day16.Run ( run ) where

import Lib ( req )

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Tunnels = Map String (Int, [String])
data Valve = Valve {
    rate :: Int,
    tunnels :: Map String Int
} deriving Show

type DistQueue = (Set (Int, String), Map String (Int, String))
startFrom :: Tunnels -> String -> DistQueue
startFrom vmap v = (Set.singleton (0, v), Map.insert v (0, "").Map.fromList.map (\n -> (n, (maxBound, ""))).Map.keys $ vmap)

dijkstra :: Tunnels -> DistQueue -> Map String Int
dijkstra valves (prio, dmap) = case Set.minView prio of
    Nothing -> Map.map (\(d, _) -> (d + 1)) dmap
    Just ((d, curr), queue) -> if fst (dmap ! curr) /= d 
        then dijkstra valves (queue, dmap)
        else dijkstra valves $ foldr 
            (\next vert@(q, dm) -> case Map.lookup next dm >>= \(dn, _) -> Just (d < dn) of
                Just True -> (Set.insert (d+1, next) q, Map.insert next (d+1, curr) dm)
                _ -> vert)
            (queue, dmap)
            (snd $ valves ! curr)

parse :: String -> Tunnels
parse = Map.fromList.map (\s -> 
    let w = words s in (w !! 1,
        ((read.drop (length "rate=").init $ w !! 4), 
         (map (filter (/= ',')) $ drop 9 w)))).lines

valveMap :: String -> Map String Valve
valveMap input = let ts = parse input in 
    Map.mapWithKey (\s (r, _) -> Valve r $ dijkstra ts (startFrom ts s)) ts

activeValves :: Map String Valve -> [String]
activeValves = Map.keys.Map.filter ((>0).rate)

search :: Map String Valve -> Int -> [String] -> Int
search valves maxt = search' ("AA", 0, 0, 0)
    where search' (_, tm, rt, tot) [] = tot + (maxt - tm) * rt
          search' state going = maximum $ mapWithRest (handle state) going
          handle (curr, tm, rt, tot) next rest
            | tm + dt < maxt = search' (next, tm + dt, rt + (rate $ valves ! next), tot + dt*rt) rest
            | otherwise      = tot + (maxt - tm) * rt
            where dt = (tunnels $ valves ! curr) ! next

mapWithRest :: (a -> [a] -> b) -> [a] -> [b]
mapWithRest f = mapWithRest' []
    where mapWithRest' left (r : right) = f r (left ++ right) : mapWithRest' (left ++ [r]) right
          mapWithRest' _ _ = []

divide :: [String] -> [([String], [String])]
divide [] = []
divide (x : xs) = tail $ go ([x], []) xs
    where go a [] = [a] 
          go (left, right) (r : rs) = go (r : left, right) rs ++ go (left, r : right) rs

part1 :: String -> Int
part1 s = search vs 30 $ activeValves vs
    where vs = valveMap s

part2 :: String -> Int
part2 s = maximum.fmap (\(a, b) -> search vs 26 a + search vs 26 b).divide.activeValves $ vs
    where vs = valveMap s

run :: IO ()
run = do
    r <- req 16
    putStrLn "--- Day 16 ---"
    putStrLn.((++) "part 1: ").show $ part1 r
    putStrLn.((++) "part 2: ").show $ part2 r