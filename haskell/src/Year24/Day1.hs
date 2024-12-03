{-# LANGUAGE ImportQualifiedPost #-}

module Year24.Day1 (main) where

import Data.List (sort)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Lib (requestDay)

parse :: T.Text -> ([Int], [Int])
parse = pairsToLists . map toTuple . T.lines
  where
    toTuple x = let ws = T.words x in (read . T.unpack $ head ws, read . T.unpack $ (!! 1) ws)
    pairsToLists = foldr f ([], [])
    f (a, b) (as, bs) = (a : as, b : bs)

partOne :: ([Int], [Int]) -> Int
partOne (as, bs) = sum $ zipWith (\a b -> abs (a - b)) (sort as) (sort bs)

partTwo :: ([Int], [Int]) -> Int
partTwo (as, bs) = sum $ map (\a -> a * fromMaybe 0 (count bs M.!? a)) as
  where
    count = foldr f M.empty
    f x = M.insertWith (+) x 1

main :: IO ()
main = do
  input <- requestDay 1
  let twoLists = parse input
  print . partOne $ twoLists
  print . partTwo $ twoLists
