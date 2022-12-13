{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day11.Run (run) where

import Data.List (sort)
import Data.List.Split (splitOn, chunksOf)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vec
import Lib (file)

data Monkey = Monkey {
    getOper :: Int -> Int,
    getTest :: Int,
    getNext :: Int,
    getPrev :: Int
}

type Items = (Vector Int, Vector (Vector Int))

instance Show Monkey where 
    show (Monkey _ t n p) = "Monkey (" ++ show t ++ ", "++ show n ++ ", " ++ show p ++ ")"

parse :: String -> (Vector Monkey, Vector (Vector Int))
parse = Vec.unzip.Vec.fromList.map createMonkey.chunksOf 7.lines

createMonkey :: [String] -> (Monkey, Vector Int)
createMonkey s = (Monkey oper test next prev, items)
    where items = Vec.fromList.map read.splitOn ", ".drop (length "  Starting items: ") $ s !! 1
          test = read.drop (length "  Test: divisible by ") $ s !! 3
          next = read.drop (length "    If true: throw to monkey ") $ s !! 4
          prev = read.drop (length "    If false: throw to monkey ") $ s !! 5
          oper = let w = words.drop (length "  Operation: new = old ") $ s !! 2
            in case w !! 0 of
                "+" -> (+ (read $ w !! 1))
                "*" -> if (w !! 1) == "old" then (^(2::Int)) else (* (read $ w !! 1))
                _ -> id


worry :: (Int -> Int) -> Monkey -> Int -> (Int, Int)
worry f (Monkey oper test n p) x = let y = f.oper $ x in if (y `rem` test) == 0 then (n, y) else (p, y)

turn :: (Int -> Int) -> Items -> (Int, Monkey) -> Items
turn f (counts, items) (mki, mnk) = (
    counts // [(mki, length x + counts ! mki )],
    foldr (\(i, k) its -> its // [(i, its ! i `Vec.snoc` k)])
          (items // [(mki, Vec.empty)])
          (foldr (\k -> (:) $ worry f mnk k) [] x))
    where x = items ! mki
          
playRound :: (Int -> Int) -> Vector Monkey -> Items -> Items
playRound f mks = flip (foldl $ turn f) (Vec.zip (Vec.fromList [0..length mks]) mks)


part1 :: [Char] -> Int
part1 r = (\cs -> cs !! 0 * cs !! 1).reverse.sort.Vec.toList.fst.(!! 20).iterate (playRound (`quot` 3) monkes) $ start
    where (monkes, items) = parse r
          start = (Vec.replicate (length monkes) 0, items)

part2 :: [Char] -> Int
part2 r = (\cs -> cs !! 0 * cs !! 1).reverse.sort.Vec.toList.fst.(!! 10000).iterate (playRound (`mod` q) monkes) $ start
    where (monkes, items) = parse r
          q = product $ Vec.map getTest monkes
          start = (Vec.replicate (length monkes) 0, items)

run :: IO ()
run = do
    putStrLn "--- Day 11 ---"
    r <- file 11
    putStrLn.((++) "part 1: ").show.part1 $ r
    putStrLn.((++) "part 2: ").show.part2 $ r