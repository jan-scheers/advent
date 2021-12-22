module Seventeen where

import Data.List

type Position = (Int, Int)

launch :: (Int, Int) -> [Position]
launch (vx, vy) = scanl (\(x,y) i -> (x + max 0 (vx-i), y + vy -i)) (0,0) [0..]

calcminX :: Int -> Int
calcminX x_min = ceiling $ (sqrt (8 * fromIntegral x_min + 1) - 1) / 2

going :: (Int, Int) -> Position -> Bool
going (x_min, y_max) (x, y) = x < x_min || y > y_max

hit :: (Int, Int, Int, Int) -> Position -> Bool
hit (x_min, x_max, y_min, y_max) (x, y) = x_min <= x && x <= x_max && y_min <= y && y <= y_max

run :: (Int, Int, Int, Int) -> [Position]
run (x_min, x_max, y_min, y_max) = filter willhit space
  where space = [(x,y) | x <- [xmin..x_max], y <- [-ylim..ylim-1]]
        ylim = abs y_min
        xmin = calcminX x_min
        willhit = hit (x_min, x_max, y_min, y_max).head.dropWhile (going (x_min, y_max)).launch


