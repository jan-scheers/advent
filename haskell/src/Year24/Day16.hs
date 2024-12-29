{-# LANGUAGE TupleSections #-}

module Year24.Day16 (main) where

import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.PSQueue (Binding ((:->)))
import qualified Data.PSQueue as PSQ
import qualified Data.Set as Set
import qualified Data.Text as T
import Lib (Pos, delta, dirToChar, east, requestDay)
import qualified Matrix as Mat

_test :: IO T.Text
_test = T.pack <$> readFile "./src/Year24/Day16.txt"

main :: IO ()
main = do
  putStrLn "Day 16"
  field <- parse <$> requestDay 16
  let (cost, ps) = solve field
  print cost
  print $ length ps

type Field = Mat.Matrix Char

parse :: T.Text -> Field
parse = Mat.fromLists . map T.unpack . T.lines

findStart :: Mat.Matrix Char -> (Int, Int)
findStart = fromJust . Mat.findIndex (== 'S')

findEnd :: Mat.Matrix Char -> (Int, Int)
findEnd = fromJust . Mat.findIndex (== 'E')

type PosDir = (Pos, Int)

type State = Map.Map PosDir Int

_dijkstra :: Field -> PosDir -> State
_dijkstra field start = go (PSQ.singleton start 0) Map.empty
  where
    go :: PSQ.PSQ (Pos, Int) Int -> State -> State
    go queue best = case PSQ.findMin queue of
      Nothing -> best
      Just (posDir :-> cost) -> go queue' visited'
        where
          visited' = Map.insert posDir cost best
          queue' =
            foldr
              (\(pd, c) q -> PSQ.insertWith min pd (cost + c) q)
              (PSQ.deleteMin queue)
              (filter (\(p, _) -> Map.notMember p best) (neighbors field posDir))

neighbors :: Field -> PosDir -> [(PosDir, Int)]
neighbors field (pos, dir) =
  let next = pos + delta dir
   in [((next, dir), 1) | field Mat.! next /= '#']
        ++ map (,1000) [(pos, (dir + 1) `mod` 4), (pos, (dir - 1) `mod` 4)]

_partOne :: Field -> Int
_partOne field = minimum costs
  where
    best = _dijkstra field (findStart field, east)
    costs = mapMaybe (\dir -> Map.lookup (findEnd field, dir) best) [0 .. 3]

type Path = [PosDir]

type Best = Map.Map PosDir (Int, [PosDir])

dfs :: Field -> PosDir -> Best
dfs field start = go (0, start, start) Map.empty
  where
    go :: (Int, PosDir, PosDir) -> Best -> Best
    go (cost, prev, curr) best = case Map.lookup curr best of
      Nothing -> explore
      Just (bestCost, prevs) -> case compare cost bestCost of
        LT -> explore
        EQ -> Map.insert curr (cost, prev : prevs) best
        GT -> best
      where
        explore :: Best
        explore =
          foldr
            (\(next, c) -> go (c + cost, curr, next))
            (Map.insert curr (cost, [prev]) best)
            (neighbors field curr)

follow :: Best -> PosDir -> [Path]
follow best = go
  where
    go :: PosDir -> [Path]
    go curr = map (curr :) $ case Map.lookup curr best of
      Nothing -> []
      Just (_, prevs) -> do
        prev <- prevs
        path <- if prev == curr then [[]] else go prev
        return (prev : path)

_draw :: Path -> Field -> Field
_draw path field = foldr (\(pos, dir) -> Mat.set (dirToChar dir) pos) field path

solve :: Field -> (Int, Set.Set Pos)
solve field = (minCost, Set.fromList positions)
  where
    best = dfs field (findStart field, east)
    (minCost, ends) = foldr (findMinima . (findEnd field,)) (maxBound, []) [0 .. 3]
    findMinima key acc@(cost, keys) =
      let c = fromMaybe maxBound (Map.lookup key best >>= Just . fst)
       in case compare c cost of
            LT -> (c, [key])
            EQ -> (c, key : keys)
            GT -> acc
    positions = map fst . concat $ concatMap (follow best) ends