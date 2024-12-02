module Year22.Day15.Run (run) where

import Year22.Lib (file)
import Data.List (nub, find)
import Data.Sequence (Seq (..), ViewL (..), ViewR (..), (><), (<|), (|>), spanl, viewl, viewr)
import qualified Data.Sequence as Seq


type Point = (Int, Int)
type Range = (Int, Int)
type Circle = (Point, Int)

parse :: String -> [(Point, Point)]
parse = map ((\ws -> ((rd.init $ ws !! 2, rd.init $ ws !! 3), (rd.init $ ws !! 8, rd $ ws !! 9))).words).lines
    where rd = read.drop 2

circle :: (Point, Point) -> Circle
circle (mp@(a, b), (c, d)) = (mp, abs (a - c) + abs (b - d))

union :: Range -> Seq Range -> Seq Range
union r@(r0, r1) ranges = fore >< range <| aft
    where (fore, tl) = spanl ((< r0 - 1).snd) ranges 
          (mid, aft) = spanl ((<= r1 + 1).fst) tl
          range = case (viewl mid, viewr mid) of 
            ((a, _) :< _, _ :> (_, b)) -> (min a r0, max b r1)
            _ -> r

crop :: Range -> Seq Range -> Seq Range
crop (r0, r1) ranges = let mid = Seq.takeWhileR ((<= r1).fst).Seq.dropWhileL ((< r0).snd) $ ranges in
    case viewl mid of 
    EmptyL -> Seq.empty
    ((s0, s1) :< tl) -> case viewr tl of
        EmptyR -> Seq.singleton (max r0 s0, min r1 s1)
        (md :> (t0, t1)) -> ((max r0 s0, s1) <| md) |> (t0, min r1 t1)

view :: Int -> [Circle]-> Seq Range
view row = foldr addCircle Seq.empty
    where addCircle ((x, y), len) rs = let w = len - abs (row - y) in 
            if w < 0 then rs else (x - w, x + w) `union` rs


part1 :: Int -> String -> Int
part1 row input = foldr (\(x0, x1) -> (x1 - x0 + 1 +)) 0 ranges 
                - length (filter (`isIn` ranges) beacons)
    where points = parse input 
          beacons = nub.map fst.filter ((==row).snd).map snd $ points      
          ranges = view row.map circle $ points

isIn :: Int -> Seq Range -> Bool
isIn x rs = case Seq.dropWhileL ((< x).snd) rs of
    (x0, _) :<| _ -> x0 < x
    _ -> False


part2 :: (Int, Int) -> String -> Int
part2 domain input = case find ((> 1).length.snd).rows domain.map circle.parse $ input of
    Just (y, (_, x) :<| _) -> (x + 1) * 4000000 + y
    _ -> -1

rows :: (Int, Int) -> [Circle] -> [(Int, Seq Range)]
rows dom@(x0, x1) cs = map (\y -> (y, crop dom.view y $ cs)) [i | i <- [x0..x1]]


run :: IO ()
run = do
    putStrLn "--- Day 15 ---"
    r <- file 15
    let n = 10
    putStrLn.((++) "part 1: ").show $ part1 n r
    putStrLn.((++) "part 2: ").show $ part2 (0, 2*n) r