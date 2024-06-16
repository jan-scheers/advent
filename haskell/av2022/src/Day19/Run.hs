module Day19.Run where

import Data.List.Split (chunksOf, splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Lens.Micro.Platform

ores :: Map String Int
ores = Map.fromList $ zip ["ore", "clay", "obsidian", "geode"] [0 ..]

type Recipe = [[Int]]

data State = State
  { _stTime :: Int,
    _stRobots :: [Int],
    _stStore :: [Int]
  }

stTime :: Lens' State Int
stTime = lens _stTime (\a b -> a {_stTime = b})

stRobots :: Lens' State [Int]
stRobots = lens _stRobots (\a b -> a {_stRobots = b})

stStore :: Lens' State [Int]
stStore = lens _stStore (\a b -> a {_stStore = b})

bp :: String -> [Recipe]
bp =
  map
    ( map
        ( foldr (\p -> set (ix $ ores Map.! (p !! 1)) (read $ p !! 0)) [0, 0, 0, 0]
            . chunksOf 2
            . filter (/= "and")
            . drop 4
            . words
        )
        . filter (not . null)
        . splitOn "."
        . drop 2
        . dropWhile (/= ':')
    )
    . lines

(.*) :: [Int] -> [Int] -> Int
(.*) a b = sum $ zipWith (*) a b

go :: Recipe -> Int
go rs = go' (State 0 [1, 0, 0, 0] [0, 0, 0, 0])
  where
    go' s = maximum $ ((sim (24 - view stTime s) s) ^?! (stStore . ix 3)) : fmap go' (opts' s)
    opts' = buildOptions rs

buildOptions :: Recipe -> State -> [State]
buildOptions rs s = undefined

sim :: Int -> State -> State
sim dt (State tm rb st) = State (tm + dt) rb (zipWith (+) st $ fmap (* dt) rb)

canBuild :: Int -> [Int] -> State -> Maybe State
canBuild i v s
  | all (<= 0) need = Just (over (stRobots . ix i) (+ 1) (sim 1 s))
  | otherwise = Nothing
  where
    need = zipWith (-) v (view stStore s)
    can = v (.*) need != 

-- poss = Vec.any (> 0) need &&
