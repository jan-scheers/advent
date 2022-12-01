module TwentyFive where

import Data.Matrix (Matrix)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Foldable (foldrM)

moveRight :: Vector Char -> [(Int, Char)]
moveRight v = foldr (\i acc -> if canMove i then (i, '.'):(i+1 `mod` n, '>'):acc else acc) 
                    [] [0..n - 1]
    where canMove = \i -> v ! i == '>' && v ! ((i + 1) `mod` n) == '.'
          n = length v

parse :: String -> Matrix Char
parse = M.fromLists.lines

run :: IO (Matrix Char)
run = do
    r <- readFile "data/25.csv"
    return $ parse r
