module Year24.Day13 (main) where

import Data.Bifunctor (second)
import Data.Functor ((<&>))
import qualified Data.Text as T
import Lib (requestDay)
import Numeric.LinearAlgebra

_test :: IO T.Text
_test = readFile "./src/Year24/Day13.txt" <&> T.pack

parse :: T.Text -> [(Matrix Z, Vector Z)]
parse = map (p . T.lines) . T.splitOn (T.pack "\n\n") . T.strip
  where
    p input = (tr $ (2 >< 2) $ concatMap button (take 2 input), fromList $ prize (last input))
    button = map (read . T.unpack . T.drop 2) . T.splitOn (T.pack ", ") . T.drop (length "Button A: ")
    prize = map (read . T.unpack . T.drop 2) . T.splitOn (T.pack ", ") . T.drop (length "Prize: ")

newton :: Matrix Z -> Vector Z -> Vector Z
newton m v = cmap round $ m' <\> v'
  where
    (m', v') = (cmap fromIntegral m, cmap fromIntegral v) :: (Matrix Double, Vector Double)

solve :: (Matrix Z, Vector Z) -> Z
solve (m, v) = (\x -> if norm_1 (v - m #> x) == 0 then 3 * (x ! 0) + x ! 1 else 0) $ newton m v

partOne :: [(Matrix Z, Vector Z)] -> Z
partOne = sum . map solve

partTwo :: [(Matrix Z, Vector Z)] -> Z
partTwo = sum . map (solve . second (cmap (+ 10000000000000)))

main :: IO ()
main = do
  test <- parse <$> requestDay 13
  putStrLn "Day 13 test "
  print $ partOne test
  print $ partTwo test
