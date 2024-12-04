{-# LANGUAGE OverloadedStrings #-}

module Year24.Day3 (main) where

import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Lib (requestDay)

_test :: IO T.Text
_test = return "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

parseNumber :: T.Text -> Maybe Int
parseNumber a = if 0 < T.length a && T.length a <= 3 && T.all isDigit a then Just (read $ T.unpack a) else Nothing

parseMul :: T.Text -> Maybe Int
parseMul t = parseNumber a >>= \x -> parseNumber (T.tail b) >>= \y -> Just (x * y)
  where
    (a, r) = T.breakOn "," t
    (b, _) = T.breakOn ")" r

partOne :: T.Text -> Int
partOne = sum . mapMaybe parseMul . T.splitOn "mul("

filterDont :: T.Text -> [T.Text]
filterDont "" = []
filterDont t = good : filterDont (dropUntilDo rest)
  where
    (good, rest) = T.breakOn "don't()" t
    dropUntilDo = T.drop 4 . snd . T.breakOn "do()" . T.drop 6

partTwo :: T.Text -> Int
partTwo = partOne . T.concat . filterDont

main :: IO ()
main = do
  input <- requestDay 3
  print $ partOne input
  print $ partTwo input
