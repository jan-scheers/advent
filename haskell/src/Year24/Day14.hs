module Year24.Day14 (main) where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Vector as V
import Lib (Pos, requestDay)
import qualified Matrix as M

_test :: IO T.Text
_test = T.pack <$> readFile "./src/Year24/Day14.txt"

type Bot = (Pos, Pos)

parse :: T.Text -> [Bot]
parse = map getBot . T.lines . T.strip
  where
    getBot = (\p -> (getPos $ p !! 0, getPos $ p !! 1)) . T.split (== ' ')
    getPos =
      (\p -> (read . T.unpack $ p !! 0, read . T.unpack $ p !! 1))
        . T.split (== ',')
        . T.drop 2

move :: Pos -> Int -> Bot -> Pos
move (x, y) t (p, (vx, vy)) = let (px, py) = p + (t * vx, t * vy) in (px `mod` x, py `mod` y)

count :: Pos -> [Pos] -> Int
count size = V.product . foldr countQuad (V.fromList [0, 0, 0, 0])
  where
    countQuad p cnt =
      fromMaybe cnt $
        findQuad size p >>= \i -> return $ cnt V.// [(i, (cnt V.! i) + 1)]

    findQuad (x, y) (px, py) = case (compare px (div x 2), compare py (div y 2)) of
      (LT, LT) -> Just 0
      (GT, LT) -> Just 1
      (LT, GT) -> Just 2
      (GT, GT) -> Just 3
      _ -> Nothing

partOne :: Pos -> [Bot] -> Int
partOne s = count s . map (move s 100)

score :: [Pos] -> Int
score = V.product . foldr f (V.replicate 16 0)
  where
    sixteenth (x, y) = 4 * div x 26 + div y 26
    f p cnt = cnt V.// [(sixteenth p, 1)]

draw :: (Foldable t) => (Int, Int) -> t (Int, Int) -> String
draw (m, n) ps = M.pretty . M.transpose $ M.matrix m n (\p -> if p `elem` ps then '#' else '.')

combine :: [String] -> String
combine = unlines . go . map lines
  where
    go xs = if null $ head xs then [] else (unwords . map head) xs : go (map tail xs)

-- 6644
partTwo :: (Int, Int) -> [Bot] -> IO ()
partTwo s = run 0
  where
    run :: Int -> [Bot] -> IO ()
    run i bots = do
      let ix = [1 .. 4]
      let pss = map (\k -> map (move s k) bots) ix
      putStrLn . combine $ map (draw s) pss
      print . map (+ i) $ ix
      when (i <= 10404) . run (i + 4) . zipWith (\(_, v) p -> (p, v)) bots . last $ pss

main =
  do
    putStrLn "Day 14"
    -- test <- parse <$> _test
    -- print $ partOne (11, 7) test
    input <- parse <$> requestDay 14
    print $ partOne (101, 103) input
    -- partTwo (101, 103) input
    putStrLn . draw (101, 103) $ map (move (101, 103) 6644) input
