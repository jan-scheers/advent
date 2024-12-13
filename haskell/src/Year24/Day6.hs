{-# LANGUAGE OverloadedStrings #-}

module Year24.Day6 (main) where

import Control.Monad (guard)
import Data.Functor ((<&>))
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Lib (requestDay)

_test :: IO T.Text
_test = readFile "./src/Year24/Day6.txt" <&> T.pack

type Grid = M.Map (Int, Int) Char

type Position = (Int, Int, Char)

parse :: T.Text -> ((Int, Int), Grid)
parse input = (start, grid)
  where
    matrix = V.fromList . map (V.fromList . T.unpack) . T.lines $ input
    (m, n) = (V.length matrix, V.length $ matrix V.! 0)
    start = fromJust $ find (\(i, j) -> matrix V.! i V.! j == '^') [(i, j) | i <- [0 .. m - 1], j <- [0 .. n - 1]]
    grid = M.fromList $ do
      i <- [0 .. m - 1]
      j <- [0 .. n - 1]
      let c = if (i, j) == start then '.' else matrix V.! i V.! j
      return ((i, j), c)

step :: Grid -> Position -> Maybe Position
step grid (i, j, c) = case c of
  '^' -> grid M.!? (i - 1, j) >>= \d -> return $ if d == '#' then (i, j, '>') else (i - 1, j, c)
  '>' -> grid M.!? (i, j + 1) >>= \d -> return $ if d == '#' then (i, j, 'v') else (i, j + 1, c)
  'v' -> grid M.!? (i + 1, j) >>= \d -> return $ if d == '#' then (i, j, '<') else (i + 1, j, c)
  ___ -> grid M.!? (i, j - 1) >>= \d -> return $ if d == '#' then (i, j, '^') else (i, j - 1, c)

walk :: Grid -> Position -> S.Set Position -> S.Set Position
walk grid p s =
  let s' = S.insert p s
   in case step grid p of
        Just p2 -> walk grid p2 s'
        Nothing -> s'

partOne :: Grid -> Position -> Int
partOne grid start = S.size $ S.foldr f S.empty visited
  where
    visited = walk grid start S.empty
    f (i, j, _) = S.insert (i, j)

walk' :: Position -> S.Set Position -> Grid -> Bool
walk' p s grid =
  let s' = S.insert p s
   in case step grid p of
        Just p2 -> S.member p2 s' || walk' p2 s' grid
        Nothing -> False

partTwo :: Grid -> Position -> Int
partTwo grid start@(a, b, _) = length . filter (walk' start S.empty) . map (\k -> M.insert k '#' grid) $ grids
  where
    (m, n) = foldr (\(i, j) (i', j') -> (max i i', max j j')) (0, 0) $ M.keys grid
    grids = do
      i <- [0 .. m]
      j <- [0 .. n]
      guard . not $ (i, j) == (a, b) || grid M.! (i, j) == '#'
      return (i, j)

_pretty :: Grid -> IO ()
_pretty grid = putStrLn . unlines $ map (\i -> map (\j -> grid M.! (i, j)) [0 .. n]) [0 .. m]
  where
    (m, n) = foldr (\(i, j) (i', j') -> (max i i', max j j')) (0, 0) $ M.keys grid

main :: IO ()
main = do
  input <- requestDay 6
  let ((i, j), grid) = parse input
  let start = (i, j, '^')
  print $ partOne grid start
  print $ partTwo grid start