{-# LANGUAGE OverloadedStrings #-}

module Year24.Day18 (main) where

import qualified Data.Map as Map
import Data.PSQueue (Binding ((:->)))
import qualified Data.PSQueue as PSQ
import qualified Data.Text as T
import Lib (Pos, plus, requestDay)
import qualified Matrix as Mat

_test :: IO T.Text
_test = T.pack <$> readFile "./src/Year24/Day18.txt"

main :: IO ()
main = do
  putStrLn "Day 18"
  bytes <- parse <$> requestDay 18
  partOne (1024, 70) bytes

parse :: T.Text -> [Pos]
parse = map pair . T.lines
  where
    pair line = case T.splitOn "," line of
      [x, y] -> ((+ 1) . read . T.unpack $ x, (+ 1) . read . T.unpack $ y)
      _ -> error "Invalid input format"

type Field = Mat.Matrix Char

empty :: Int -> Field
empty n = Mat.matrix (n + 3) (n + 3) (\(x, y) -> if x == 0 || y == 0 || x == n + 2 || y == n + 2 then '#' else '.')

type Best = Map.Map Pos Int

dijkstra :: Field -> Pos -> Best
dijkstra field start = go (PSQ.singleton start 0) Map.empty
  where
    go :: PSQ.PSQ Pos Int -> Best -> Best
    go queue best = case PSQ.findMin queue of
      Nothing -> best
      Just (pos :-> cost) -> go queue' best'
        where
          best' = Map.insert pos cost best
          next = filter (`Map.notMember` best) . filter (\p -> field Mat.! p /= '#') . plus
          queue' =
            foldr
              (\p q -> PSQ.insertWith min p (cost + 1) q)
              (PSQ.deleteMin queue)
              (next pos)

partOne :: Pos -> [Pos] -> IO ()
partOne (m, n) bytes = do
  print ("Part One" :: String)
  let field = foldr (Mat.set '#') (empty n) (take m bytes)
  putStrLn . Mat.pretty . Mat.transpose $ field
  let b = dijkstra field (1, 1) Map.! (n + 1, n + 1)
  print b
  partTwo n (drop m bytes) field

partTwo :: Int -> [Pos] -> Field -> IO ()
partTwo _ [] _ = pure ()
partTwo n (byte : bytes) field = do
  let field' = Mat.set '#' byte field
  let sol = dijkstra field' (1, 1)
  if (n + 1, n + 1) `Map.notMember` sol
    then putStrLn $ show (fst byte - 1) ++ "," ++ show (snd byte - 1)
    else partTwo n bytes field'