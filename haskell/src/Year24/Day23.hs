{-# LANGUAGE OverloadedStrings #-}

module Year24.Day23 (main) where

import Control.Monad (guard)
import Data.List (intersperse, sort)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Lib (requestDay)

_test :: IO T.Text
_test = T.pack <$> readFile "./src/Year24/Day23.txt"

type Graph = Map.Map T.Text (Set.Set T.Text)

parse :: T.Text -> Graph
parse = foldr ((\(a, b) -> Map.insertWith Set.union b (Set.singleton a) . Map.insertWith Set.union a (Set.singleton b)) . conn) Map.empty . T.lines
  where
    conn line = let (a, b) = T.breakOn "-" line in (a, T.tail b)

main :: IO ()
main = do
  putStrLn "Day 23"
  graph <- parse <$> requestDay 23
  print $ partOne graph
  putStrLn . T.unpack $ partTwo graph

partOne :: Graph -> Int
partOne = length . filter (any ("t" `T.isPrefixOf`)) . findTriplets

findTriplets :: Graph -> [[T.Text]]
findTriplets graph = do
  let nodes = zip (Map.keys graph) [1 ..]
  (a, i) <- nodes
  (b, j) <- drop i nodes
  (c, _) <- drop j nodes
  guard $ c `elem` (graph Map.! b) && c `elem` (graph Map.! a) && b `elem` (graph Map.! a)
  return [a, b, c]

-- partTwo :: Graph -> T.Text
partTwo :: Graph -> T.Text
partTwo graph =
  T.concat . intersperse "," . sort . Set.toList . snd . maximum $
    map best (Map.keys graph)
  where
    best node = let s = dfs graph node in (Set.size s, s)

dfs :: Graph -> T.Text -> Set.Set T.Text
dfs graph start = snd . maximum . map (\s -> (Set.size s, s)) $ solution
  where
    solution = foldr reduce [Set.singleton start] (Set.toList (graph Map.! start))
    reduce node = foldr (\curr -> (fully graph node curr ++)) []

fully :: Graph -> T.Text -> Set.Set T.Text -> [Set.Set T.Text]
fully graph node curr = if Set.size sect == Set.size curr then [next] else [curr, next]
  where
    sect = graph Map.! node `Set.intersection` curr
    next = Set.insert node sect