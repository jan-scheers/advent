{-# LANGUAGE LambdaCase #-}

module Fifteen where

import qualified Data.ByteString.Char8 as C
import Data.Maybe (mapMaybe)
import Data.Matrix hiding ((!))
import Data.Map (Map, (!))
import qualified Data.Map as Map

type Position = (Int, Int)
type Dist = Int

get :: Matrix a -> Position -> a
get gr p = uncurry unsafeGet p gr

weight :: C.ByteString -> Matrix Int
weight = fromLists.(map.map $ \c -> read [c]).lines.C.unpack

tile :: Int -> Matrix Int -> Matrix Int
tile k mat = foldr1 (<->) (take k rows)
  where f = \a b -> mod (a+b-1) 9 + 1
        tiles = map (\a -> fmap (f a) mat) [0..]
        rows = map (\a -> foldr1 (<|>) (take k. drop a $ tiles)) [0..]

setStart :: Matrix Int -> Matrix Int
setStart = unsafeSet 0 (1,1)

dijkstra :: Matrix Int -> Map Position Dist -> Map Position Dist -> Map Position Dist
dijkstra wmat queue seen 
  | Map.null queue = seen
--  | cur_pos == (nrows wmat, ncols wmat) = seen'
  | otherwise = dijkstra wmat queue' seen'
  where ((cur_pos, cur_dist), qtail) = pop queue
        cur_wt = get wmat cur_pos
        cur_dist' = cur_dist + cur_wt
        nbrs = neighbors wmat seen cur_pos
        queue' = foldr (updateq (cur_pos, cur_dist')) qtail nbrs
        seen' = Map.insert cur_pos cur_dist' seen

pop :: Map Position Dist -> ((Position, Dist), Map Position Dist)
pop queue = ((s_pos, s_dist), Map.delete s_pos queue)
  where shorter = \(pos, dist) (m_pos, m_dist) -> if dist < m_dist then (pos, dist) else (m_pos, m_dist)
        (s_pos, s_dist) = foldr1 shorter (Map.toList queue)

neighbors :: Matrix Int -> Map Position Dist -> Position -> [Position]
neighbors wmat seen (i,j) = filter allow ps
  where allow = \p -> safe p && Map.notMember p seen
        safe  = \(a,b) -> 1 <= a && a <= nrows wmat && 1 <= b && b <= ncols wmat 
        ps = [(i-1,j),(i+1,j),(i,j-1),(i,j+1)]

updateq :: (Position, Dist) -> Position -> Map Position Dist -> Map Position Dist
updateq (cur_pos, cur_dist) = Map.alter addOrChange
  where addOrChange = \case
          Nothing -> Just cur_dist
          Just prev_dist -> Just (min cur_dist prev_dist)

run :: C.ByteString -> Dist
run r = dk ! (nrows wt, ncols wt)
  where wt = setStart.tile 5.weight $ r
        dk = dijkstra wt (Map.singleton (1,1) 0) Map.empty
