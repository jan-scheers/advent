{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Year22.Day16.Opti () where

import Data.Vector (Vector, (//))
import qualified Data.Vector as Vec
import Data.Map (Map, (!))
import Data.List (sortOn)

data Valve = Valve {
    rate :: Int,
    tunnels :: Map String Int
} deriving Show

cost :: Map String Valve -> Int -> Vector String -> Int
cost vs maxt sol
    | null sol = -1
    | otherwise = c + (maxt - t) * rt
    where (c, rt, t, _) = Vec.foldl (eval vs maxt) (0, 0, 0, "AA") sol

eval :: Map String Valve -> Int -> (Int, Int, Int, String) -> String -> (Int, Int, Int, String)
eval valves maxt (c, rt, t, pr) cr = (c + rt * dt, rt + rate (valves ! cr), t + dt, cr)
    where len = (tunnels (valves ! pr)) ! cr
          dt = if t + len >= maxt then maxt - t else len

swap :: Vector a -> (Int, Int) -> Vector a
swap  v (i, j)= v // [(i, Vec.unsafeIndex v j),(j, Vec.unsafeIndex v i)]

shift :: Vector a -> Int -> Vector a
shift v r = let (left, right) = Vec.splitAt r v in right Vec.++ left

climb :: Map String Valve -> Int -> Vector String -> (Vector String, Int)
climb vs maxt sol 
    | bestCost <= currCost = (sol, currCost)
    | otherwise = climb vs maxt bestSol
        where (bestCost, bestSol) = head.sortOn (negate.fst).map (\s -> (cost vs maxt s, s)).opts1 $ sol
              currCost = cost vs maxt sol

opts1 :: Vector String -> [Vector String]
opts1 sol = concat [swaps, rswaps, shifts, rshifts]
    where n = length sol
          swaps = swap sol <$> [(i, j) | i <- [0..n - 1], j <- [0..n - 1], i < j]
          rswaps = swap (Vec.reverse sol) <$> [(i, j) | i <- [0..n - 1], j <- [0..n - 1], i < j]
          shifts = shift sol <$> [i | i <- [1..n-1]]
          rshifts = shift (Vec.reverse sol) <$> [i | i <- [0..n-1]]


climb2 :: Map String Valve -> Vector String 
    -> ((Vector String, Vector String), Int)
climb2 vs = head.sortOn (negate.snd).map (\(a, b) -> let ((s0, c0), (s1, c1)) = (climb vs 26 a, climb vs 26 b) in ((s0, s1), c0 + c1)).opts2

opts2 :: Vector a -> [(Vector a, Vector a)]
opts2 = map (\(a, b) -> (Vec.fromList a, Vec.fromList b))
    .filter (\(a, b) -> not (null a || null b))
    .Vec.foldr (\s -> concatMap (\(a, b) -> [(s:a, b), (a, s:b)])) [([], [])]