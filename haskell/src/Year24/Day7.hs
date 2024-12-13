{-# LANGUAGE OverloadedStrings #-}

module Year24.Day7 (main) where

import Control.Monad (guard)
import Data.Functor ((<&>))
import Data.List (concatMap, find)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Lib (requestDay)

_test :: IO T.Text
_test = readFile "./src/Year24/Day7.txt" <&> T.pack

parse :: T.Text -> [(Int, [Int])]
parse = map parseLine . T.lines
  where
    parseLine line = (read . T.unpack $ a, map (read . T.unpack) . T.splitOn " " . T.drop 2 $ b)
      where
        (a, b) = T.breakOn ": " line

sums1 :: [Int] -> [Int]
sums1 = foldl f []
  where
    f :: [Int] -> Int -> [Int]
    f [] a = [a]
    f xs a = xs >>= \x -> [x + a, x * a]

sums2 :: [Int] -> [Int]
sums2 = foldl f []
  where
    f :: [Int] -> Int -> [Int]
    f [] a = [a]
    f xs a = xs >>= \x -> [x + a, x * a, read $ show x ++ show a]

partOne :: [(Int, [Int])] -> Int
partOne = sum . map (\(s, eq) -> if could s eq then s else 0)
  where
    could s eq = s `elem` sums1 eq

partTwo :: [(Int, [Int])] -> Int
partTwo = sum . map (\(s, eq) -> if could s eq then s else 0)
  where
    could s eq = s `elem` sums2 eq

main :: IO ()
main = do
  input <- requestDay 7
  print . partOne . parse $ input
  print . partTwo . parse $ input