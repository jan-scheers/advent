{-# LANGUAGE ImportQualifiedPost #-}

module Year24.Day1 (main) where

import Data.List (sort)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Lib (requestDay)

parse :: T.Text -> ([Int], [Int])
parse = pairsToLists . map toIntPairs . T.lines
  where
    toIntPairs = (\pair -> (toInt $ head pair, toInt $ last pair)) . T.words
    toInt = read . T.unpack
    pairsToLists = foldr f ([], [])
    f (a, b) (as, bs) = (a : as, b : bs)

partOne :: ([Int], [Int]) -> Int
partOne (as, bs) = sum $ zipWith (\a b -> abs (a - b)) (sort as) (sort bs)

partTwo :: ([Int], [Int]) -> Int
partTwo (as, bs) = sum $ map similarity as
  where
    similarity a = a * fromMaybe 0 (count bs M.!? a)
    count = foldr f M.empty
    f x = M.insertWith (+) x 1

main :: IO ()
main = do
  input <- requestDay 1
  let twoLists = parse input
  print . partOne $ twoLists
  print . partTwo $ twoLists
