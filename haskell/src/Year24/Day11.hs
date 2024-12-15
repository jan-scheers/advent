{-# LANGUAGE TupleSections #-}

module Year24.Day11 (main) where

import qualified Data.Map as M
import qualified Data.Text as T
import Lib (requestDay)

type Count = M.Map Int Int

_test :: IO T.Text
_test = return $ T.pack "125 17"

parse :: T.Text -> [Int]
parse = map (read . T.unpack) . T.words . T.strip

step :: Int -> [Int]
step 0 = [1]
step i =
  let s = show i
   in if even $ length s
        then map read [take (length s `div` 2) s, drop (length s `div` 2) s]
        else [i * 2024]

partOne :: [Int] -> Int
partOne stones = length $ iterate (concatMap step) stones !! 25

step' :: Count -> Count
step' = foldr (uncurry (M.insertWith (+))) M.empty . nextStones . M.toList
  where
    nextStones :: [(Int, Int)] -> [(Int, Int)]
    nextStones = concatMap (\(k, c) -> map (,c) (step k))

partTwo :: [Int] -> Int
partTwo stones = sum $ iterate step' (M.fromList $ map (,1) stones) !! 75

main :: IO ()
main = do
  putStrLn "Day 11"
  stones <- parse <$> requestDay 11
  print $ partOne stones
  print $ partTwo stones
