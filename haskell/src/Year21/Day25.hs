
module Year21.Day25 (run) where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.Foldable (foldrM)

moveRight :: V.Vector Char -> [(Int, Char)]
moveRight v = foldr (\i acc -> if canMove i then (i, '.'):(i+1 `mod` n, '>'):acc else acc) 
                    [] [0..n - 1]
    where canMove = \i -> v V.! i == '>' && v V.! ((i + 1) `mod` n) == '.'
          n = length v

parse :: String -> M.Matrix Char
parse = M.fromLists.lines

run :: IO (M.Matrix Char)
run = do
    r <- readFile "data/25.csv"
    return $ parse r
