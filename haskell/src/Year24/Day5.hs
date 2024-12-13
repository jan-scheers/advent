{-# LANGUAGE OverloadedStrings #-}

module Year24.Day5 (main) where

import Data.Functor ((<&>))
import Data.List (sortBy)
import qualified Data.Text as T
import qualified Data.Vector as V
import Lib (requestDay)

_test :: IO T.Text
_test = readFile "Day5.txt" <&> T.pack

type RuleBook = [(Int, Int)]

type Pages = V.Vector Int

parse :: T.Text -> (RuleBook, [Pages])
parse input = (parseRules rules, parsePages $ T.drop 2 pages)
  where
    (rules, pages) = T.breakOn "\n\n" . T.strip $ input
    parsePages :: T.Text -> [V.Vector Int]
    parsePages = map (V.fromList . map (read . T.unpack) . T.splitOn ",") . T.lines

    parseRules :: T.Text -> RuleBook
    parseRules = map parseRule . T.lines
      where
        parseRule :: T.Text -> (Int, Int)
        parseRule line = (read $ T.unpack a, read $ T.unpack $ T.drop 1 b)
          where
            (a, b) = T.breakOn "|" line

order :: RuleBook -> Int -> Int -> Ordering
order rules a b = foldr f EQ rules
  where
    f (x, y) acc
      | a == x && b == y = LT
      | b == x && a == y = GT
      | otherwise = acc

sort :: RuleBook -> V.Vector Int -> V.Vector Int
sort rules = V.fromList . sortBy (order rules) . V.toList

partOne :: RuleBook -> [Pages] -> Int
partOne rules = sum . map (\m -> if sort rules m == m then m V.! (V.length m `div` 2) else 0)

partTwo :: RuleBook -> [Pages] -> Int
partTwo rules = sum . map (\m -> if sort rules m == m then 0 else sort rules m V.! (V.length m `div` 2))

main :: IO ()
main = do
  input <- requestDay 5
  let (rules, manuals) = parse input
  print $ partOne rules manuals
  print $ partTwo rules manuals