{-# LANGUAGE OverloadedStrings #-}

module Year24.Day23 (main) where

import Control.Monad (guard)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Debug.Trace (traceShow)
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
  graph <- parse <$> _test
  print $ partOne graph
  print $ graph Map.! "ka"
  print $ dfs graph "ka"

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

dfs :: Graph -> T.Text -> [Set.Set T.Text]
dfs graph start = foldr reduce [Set.singleton start] (Set.toList (graph Map.! start))
  where
    reduce node acc = reduce' . traceShow (node, acc, set) $ acc
      where
        set = Set.insert node $ graph Map.! node
        reduce' [] = []
        reduce' (s : ss) =
          let s' = Set.insert node s
              s'' = Set.intersection set s'
           in if traceShow (s', s'') length s' == length s'' then s'' : reduce' ss else s : s'' : reduce' ss
