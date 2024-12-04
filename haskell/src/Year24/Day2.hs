module Year24.Day2 (main) where

import qualified Data.Text as T
import Data.Vector ((!))
import qualified Data.Vector as V
import Lib (requestDay)

_test :: IO T.Text
_test =
  return
    . T.pack
    . unlines
    $ [ "7 6 4 2 1",
        "1 2 7 8 9",
        "9 7 6 2 1",
        "1 3 2 4 5",
        "8 6 4 4 1",
        "1 3 6 7 9"
      ]

parse :: T.Text -> V.Vector (V.Vector Int)
parse = V.fromList . map readLine . T.lines
  where
    readWord = read . T.unpack
    readLine = V.fromList . map readWord . T.words

checkSafe :: V.Vector Int -> Bool
checkSafe v = all safe is
  where
    is = take (V.length v - 1) [0 ..]
    safe i = let diff = dir * (v ! (i + 1) - v ! i) in 0 < diff && diff < 4
    dir = if v ! 1 > v ! 0 then 1 else -1

partOne :: V.Vector (V.Vector Int) -> Int
partOne = V.length . V.filter checkSafe

dropEachIndex :: V.Vector Int -> [V.Vector Int]
dropEachIndex v = foldr f [v] is
  where
    is = take (V.length v) [0 ..]
    f i acc = dropAt i : acc
    dropAt i = V.take i v V.++ V.drop (i + 1) v

partTwo :: V.Vector (V.Vector Int) -> Int
partTwo = V.length . V.filter (any checkSafe . dropEachIndex)

main :: IO ()
main = do
  input <- requestDay 2
  let vecs = parse input
  print $ partOne vecs
  print $ partTwo vecs
