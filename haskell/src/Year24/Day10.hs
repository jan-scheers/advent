module Year24.Day10 (main) where

import Control.Monad (guard)
import Data.Functor ((<&>))
import qualified Data.Text as T
import Lib (requestDay)
import qualified Matrix as M

_test :: IO T.Text
_test = readFile "./src/Year24/Day10.txt" <&> T.pack

parse :: T.Text -> M.Matrix Int
parse = M.fromLists . map (map (\c -> read [c]) . T.unpack) . T.lines

type Pos = (Int, Int)

next :: M.Matrix Int -> M.Matrix Bool -> Pos -> [Pos]
next field seen (i, j) =
  if val == 9
    then []
    else do
      nextPos@(i', j') <- [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]
      guard $ 0 <= i' && i' < m
      guard $ 0 <= j' && j' < n
      guard $ not $ seen M.! nextPos
      guard $ field M.! nextPos == val + 1
      return nextPos
  where
    (m, n) = M.shape field
    val = field M.! (i, j)

dfs :: M.Matrix Int -> (Int, Int) -> Int
dfs field pos = count $ go field pos empty
  where
    (m, n) = M.shape field
    empty = M.matrix m n (const False)
    count = M.ifoldr (\(i, j) s c -> if s && (M.get i j field == 9) then c + 1 else c) 0

go :: M.Matrix Int -> (Int, Int) -> M.Matrix Bool -> M.Matrix Bool
go field pos seen = foldr (go field) seen' nextPos
  where
    seen' = M.set True pos seen
    nextPos = next field seen' pos

partOne :: M.Matrix Int -> Int
partOne field = M.ifoldr (\pos v c -> if v == 0 then c + dfs field pos else c) 0 field

partTwo :: M.Matrix Int -> Int
partTwo field = sum $ do
  i <- [0 .. m - 1]
  j <- [0 .. n - 1]
  let pos = (i, j)
  guard $ field M.! pos == 0
  return $ go2 field pos
  where
    (m, n) = M.shape field

go2 :: M.Matrix Int -> (Int, Int) -> Int
go2 field pos
  | field M.! pos == 9 = 1
  | null nextPos = 0
  | otherwise = sum $ map (go2 field) nextPos
  where
    nextPos = next2 field pos

next2 :: M.Matrix Int -> Pos -> [Pos]
next2 field (i, j) = do
  nextPos@(i', j') <- [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]
  guard $ 0 <= i' && i' < m
  guard $ 0 <= j' && j' < n
  guard $ field M.! nextPos == val + 1
  return nextPos
  where
    (m, n) = M.shape field
    val = field M.! (i, j)

main :: IO ()
main = do
  putStrLn "Day 10"
  field <- requestDay 10 <&> parse
  print $ partOne field
  print $ partTwo field