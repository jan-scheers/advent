{-# LANGUAGE OverloadedStrings #-}

module Year24.Day4 (main) where

import Control.Monad (guard)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Vector as V
import Lib (requestDay)

_test :: IO T.Text
_test =
  return $
    T.unlines
      [ "MMMSXXMASM",
        "MSAMXMSMSA",
        "AMXSXMAAMM",
        "MSAMASMSMX",
        "XMASAMXAMM",
        "XXAMMXXAMA",
        "SMSMSASXSS",
        "SAXAMASAAA",
        "MAMMMXMMMM",
        "MXMXAXMASX"
      ]

_test2 :: Grid Char
_test2 = V.fromList [V.fromList "ABC", V.fromList "DEF", V.fromList "GHI"]

type Grid a = V.Vector (V.Vector a)

parse :: T.Text -> Grid Char
parse = V.fromList . map (V.fromList . T.unpack) . T.lines

transpose :: Grid a -> Grid a
transpose a = V.generate (V.length a) $ \i -> V.generate (V.length $ a V.! 0) $ \j -> a V.! j V.! i

diagonals :: Grid a -> Grid a
diagonals a = V.fromList $ map (V.fromList . diag) [0 .. m + n - 2]
  where
    (m, n) = (V.length a, V.length $ a V.! 0)
    diag i = mapMaybe (\j -> (a V.!? j) >>= (\row -> row V.!? (i - j))) [0 .. i]

getDiagonals :: Grid a -> [Grid a]
getDiagonals a = do
  f <- [id, V.map V.reverse]
  g <- [diagonals, diagonals . V.map V.reverse]
  return $ f . g $ a

getRotations :: Grid a -> [Grid a]
getRotations = ([id, V.map V.reverse] <*>) . ([id, transpose] <*>) . pure

-- Three versions, each shorter than the rest
_getRotations1 :: Grid a -> [Grid a]
_getRotations1 a = do
  f <- [id, V.map V.reverse]
  g <- [diagonals, diagonals . V.map V.reverse]
  return $ f . g $ a

_getRotations2 :: Grid a -> [Grid a]
_getRotations2 a = [f . g $ a | f <- [id, V.map V.reverse], g <- [diagonals, diagonals . V.map V.reverse]]

_getRotations3 :: Grid a -> [Grid a]
_getRotations3 = ([id, V.map V.reverse] <*>) . ([id, transpose] <*>) . pure

countLine :: T.Text -> V.Vector Char -> Int
countLine needle = length . T.breakOnAll needle . T.pack . V.toList

countGrid :: T.Text -> Grid Char -> Int
countGrid needle = V.sum . V.map (countLine needle)

partOne :: Grid Char -> Int
partOne g = sum . map (countGrid "XMAS") $ getRotations g ++ getDiagonals g

checkX :: Grid Char -> Int -> Int -> Bool
checkX g i j =
  g V.! i V.! j == 'M'
    && g V.! (i + 2) V.! j == 'M'
    && g V.! (i + 1) V.! (j + 1) == 'A'
    && g V.! i V.! (j + 2) == 'S'
    && g V.! (i + 2) V.! (j + 2) == 'S'

countX :: Grid Char -> Int
countX g = length $ do
  i <- [0 .. m - 3]
  j <- [0 .. n - 3]
  guard $ checkX g i j
  return ()
  where
    (m, n) = (V.length g, V.length $ g V.! 0)

partTwo :: Grid Char -> Int
partTwo = sum . map countX . getRotations

main :: IO ()
main = do
  input <- requestDay 4
  print . partOne $ parse input
  print . partTwo $ parse input