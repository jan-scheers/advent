module Year24.Day8 (main) where

import Control.Monad (guard)
import Data.Functor ((<&>))
import qualified Data.Map as Map
import qualified Data.Set as S
import qualified Data.Text as T
import Lib (requestDay)
import qualified Matrix as Mat

_test :: IO T.Text
_test = readFile "./src/Year24/Day8.txt" <&> T.pack

type Field = Mat.Matrix Char

type Pos = (Int, Int)

type Antennas = Map.Map Char [Pos]

parse :: T.Text -> (Field, Antennas)
parse text = (field, antennas)
  where
    field = Mat.fromLists . map T.unpack . T.lines . T.strip $ text
    antennas = Mat.ifoldr (\(i, j) c -> if c == '.' then id else Map.insertWith (++) c [(i, j)]) mempty field

partOne :: Field -> Antennas -> Int
partOne field = S.size . Map.foldr (flip addFrequency) S.empty
  where
    (m, n) = Mat.shape field

    resonance :: [Pos] -> [Pos]
    resonance ant = filter inRange $ do
      a <- ant
      b <- ant
      guard $ a /= b
      [2 * a - b, 2 * b - a]

    inRange :: Pos -> Bool
    inRange (i, j) = 0 <= i && i < m && 0 <= j && j < n

    addFrequency :: S.Set Pos -> [Pos] -> S.Set Pos
    addFrequency ps = foldr S.insert ps . resonance

partTwo :: Field -> Antennas -> Int
partTwo field = S.size . Map.foldr (flip addFrequency) S.empty
  where
    (m, n) = Mat.shape field

    up :: [Pos] -> [Pos]
    up ant = do
      a <- ant
      b <- ant
      guard $ a /= b
      let (dx, dy) = a - b
      takeWhile inRange [a + (k * dx, k * dy) | k <- [0 ..]]

    down :: [Pos] -> [Pos]
    down ant = do
      a <- ant
      b <- ant
      guard $ a /= b
      let (dx, dy) = b - a
      takeWhile inRange [b + (k * dx, k * dy) | k <- [0 ..]]

    inRange :: Pos -> Bool
    inRange (i, j) = 0 <= i && i < m && 0 <= j && j < n

    addFrequency :: S.Set Pos -> [Pos] -> S.Set Pos
    addFrequency ps a = foldr S.insert ps $ up a ++ down a

main :: IO ()
main = do
  input <- requestDay 8
  let (field, antennas) = parse input
  putStrLn "Day 8"
  print $ partOne field antennas
  print $ partTwo field antennas
