module Year24.Day12 (main) where

import qualified Data.Set as S
import qualified Data.Text as T
import Lib (Pos, delta, east, north, plus, requestDay)
import qualified Matrix as M

_tests :: IO [T.Text]
_tests = do
  text <- T.pack <$> readFile "./src/Year24/Day12.txt"
  return $ T.splitOn (T.pack "\n\n") text

parse :: T.Text -> M.Matrix Char
parse = M.fromLists . map T.unpack . T.lines . T.strip

partOne :: M.Matrix Char -> Int
partOne field = snd $ foldr explore (S.empty, 0) [(i, j) | i <- [0 .. m - 1], j <- [0 .. n - 1]]
  where
    (m, n) = M.shape field

    explore p state@(seen, cost) =
      if p `S.member` seen
        then state
        else
          let region = findRegion field p
              fences = findFences field region
           in (S.union seen region, cost + S.size region * sum (map length fences))

canGo :: M.Matrix Char -> (Int, Int) -> (Int, Int) -> Bool
canGo field pos next = M.inRange field next && field M.! pos == field M.! next

findRegion :: M.Matrix Char -> Pos -> S.Set Pos
findRegion field pos = go pos S.empty
  where
    go p seen =
      if S.member p seen
        then seen
        else foldr go (S.insert p seen) (filter (canGo field p) $ plus p)

type Fence = (Pos, Int)

findFences :: M.Matrix Char -> S.Set Pos -> [[Fence]]
findFences field ps = snd $ foldr go (S.empty, []) ps
  where
    go p state@(seen, fences) =
      if (p, east) `S.member` seen || canGo field p (p + delta north)
        then state
        else
          let fence = follow field (p, east)
           in (S.union seen (S.fromList fence), fence : fences)

follow :: M.Matrix Char -> Fence -> [Fence]
follow field start = start : (takeWhile (start /=) . tail . iterate follow' $ start)
  where
    follow' (pos, dir)
      | not $ canGo field pos front = (pos, goRight)
      | canGo field pos left = (left, goLeft)
      | otherwise = (front, dir)
      where
        goRight = (dir + 1) `mod` 4
        goLeft = (dir + 3) `mod` 4
        front = pos + delta dir
        left = front + delta goLeft

partTwo :: M.Matrix Char -> Int
partTwo field = snd $ foldr explore (S.empty, 0) [(i, j) | i <- [0 .. m - 1], j <- [0 .. n - 1]]
  where
    (m, n) = M.shape field

    explore p state@(seen, cost) =
      if p `S.member` seen || canGo field p (p + delta north)
        then state
        else
          let region = findRegion field p
              fences = findFences field region
           in (S.union seen region, cost + S.size region * sum (map fenceCost fences))

fenceCost :: [Fence] -> Int
fenceCost [] = 0
fenceCost ((_, start) : fs) = fenceCost' start fs
  where
    fenceCost' prev [] = if prev /= start then 1 else 0
    fenceCost' prev ((_, dir) : rest) = fenceCost' dir rest + if prev /= dir then 1 else 0

main :: IO ()
main = do
  putStrLn "Day 12"
  input <- requestDay 12
  let field = parse input
  print $ partOne field
  print $ partTwo field
