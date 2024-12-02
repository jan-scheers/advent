{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Year22.Day5.Run (run) where
import Year22.Lib (req)

parse :: String -> ([[Char]], [[Int]])
parse s = let (stacks, is) = break (=="").lines $ s 
    in (build stacks, map make $ tail is)

build :: [[Char]] -> [[Char]]
build sts = foldr 
    (zipWith (\a b -> if a == ' ' then b else a:b)) 
    (flip replicate [].length.head $ sts) 
    (map (\lv -> map (lv !!) [1, 5 .. length lv]) $ init sts)


make :: String -> [Int]
make is = (\ws -> map (read.(ws !!)) [1, 3 .. length ws]) $ words is

operate :: [[Char]] -> [Int] -> [[Char]]
operate sts [n, one, two] = let (mv, from) = splitAt n $ sts !! (one - 1) 
    in map (\(i, st) -> case () of 
        _ | i + 1 == one -> from
          | i + 1 == two -> reverse mv ++ st
          | otherwise -> st) $ zip [0..] sts
operate _ _ = []

operate2 :: [[Char]] -> [Int] -> [[Char]]
operate2 sts [n, one, two] = let (mv, from) = splitAt n $ sts !! (one - 1) 
    in map (\(i, st) -> case () of 
        _ | i + 1 == one -> from
          | i + 1 == two -> mv ++ st
          | otherwise -> st) $ zip [0..] sts
operate2 _ _ = []

run :: IO ()
run = do
    r <- req 5
    let (sts, iss) = parse r
    let (result) = foldl operate sts iss
    putStrLn $ map head result
    let (result2) = foldl operate2 sts iss
    putStrLn $ map head result2