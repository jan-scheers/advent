module Year24.Day22 (main) where

import Data.Bits (shift, xor)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Lib (requestDay)

_test :: IO T.Text
_test = T.pack <$> readFile "./src/Year24/Day22.txt"

parse :: T.Text -> [Int]
parse = map (read . T.unpack) . T.lines

main :: IO ()
main = do
  putStrLn "Day 22"
  codes <- parse <$> requestDay 22
  print $ partOne codes
  print $ partTwo codes

partOne :: [Int] -> Int
partOne = sum . map ((!! 2000) . iterate eval)

type PriceMap = Map.Map (Int, Int, Int, Int) Int

partTwo :: [Int] -> Int
partTwo =
  maximum
    . Map.elems
    . foldl (Map.unionWith (+)) Map.empty
    . map priceMap

priceMap :: Int -> PriceMap
priceMap i =
  foldl insertFirst Map.empty $
    zipper (zipWith (-) (tail prices) prices) (drop 4 prices)
  where
    prices = take 2001 $ map (`mod` 10) $ iterate eval i

    zipper (a : b : c : d : rs) (p : ps) = ((a, b, c, d), p) : zipper (b : c : d : rs) ps
    zipper _ _ = []

    insertFirst acc (k, v) = Map.alter (Just . fromMaybe v) k acc

eval :: Int -> Int
eval = h . g . f
  where
    f x = (x `xor` shift x 6) `mod` 16777216
    g x = (x `xor` shift x (-5)) `mod` 16777216
    h x = (x `xor` shift x 11) `mod` 16777216