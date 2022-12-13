{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day13.Run ( run ) where

import Lib ( req ) 
import Data.List (sortBy)
import Data.List.Split (chunksOf)
import Data.Char (isDigit)

parse :: String -> [String]
parse = foldr ((++).take 2) [].chunksOf 3.lines

cmp :: String -> String -> Ordering
cmp [] [] = EQ
cmp [] _  = LT
cmp _ []  = GT
cmp (',':left) (',':right) = cmp left right
cmp left right
    | isDigit (head l) && isDigit (head r) = case compare ((read l) :: Int) ((read r) :: Int) of
        EQ -> cmp ls rs
        ord -> ord
    | isDigit (head l) = cmp ('[':l ++ "]") r
    | isDigit (head r) = cmp l ('[':r ++ "]")
    | otherwise = case cmp (init.tail $ l) (init.tail $ r) of
        EQ -> cmp ls rs
        ord -> ord
    where (l, ls) = popNext left
          (r, rs) = popNext right
    
popNext :: String -> (String, String)
popNext [] = ([], [])
popNext cs@(c:s)
    | c == '['  = closeBracket 1 ("[", s)
    | otherwise = break (== ',') cs

closeBracket :: Int -> (String, String) -> (String, String)
closeBracket _ (s, []) = (reverse s, [])
closeBracket 0 (s, rs) = (reverse s, rs)
closeBracket i (s, c:rs)  = flip closeBracket (c:s, rs) (case c of
    '[' -> i+1
    ']' -> i-1
    _ -> i)

part1 :: String -> Int
part1 = sum.map (\(i, [l, r]) -> if GT == cmp l r then 0 else i).zip [1..].chunksOf 2.parse

part2 :: String -> Int
part2 = foldr (\(i, s) a -> if s == "[[2]]" || s == "[[6]]" then a*i else a) 1.zip [1..].sortBy cmp.((++) ["[[2]]", "[[6]]"]).parse

run :: IO ()
run = do
    putStrLn "--- Day 13 ---"
    r <- req 13
    putStrLn.((++) "part 1: ").show.part1 $ r
    putStrLn.((++) "part 1: ").show.part2 $ r