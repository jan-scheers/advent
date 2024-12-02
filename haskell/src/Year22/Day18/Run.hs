module Year22.Day18.Run (run) where

import Control.Monad (guard)
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Year22.Lib (req)

newtype Pos = Pos (Int, Int, Int) deriving (Eq, Show)

instance Ord Pos where
  compare (Pos (a0, a1, a2)) (Pos (b0, b1, b2)) = compare [a0, a1, a2] [b0, b1, b2]

(.+) :: Pos -> Pos -> Pos
(.+) (Pos (a0, a1, a2)) (Pos (b0, b1, b2)) = Pos (a0 + b0, a1 + b1, a2 + b2)

(.-) :: Pos -> Pos -> Pos
(.-) (Pos (a0, a1, a2)) (Pos (b0, b1, b2)) = Pos (a0 - b0, a1 - b1, a2 - b2)

(.*) :: Pos -> Pos -> Int
(.*) (Pos (a0, a1, a2)) (Pos (b0, b1, b2)) = a0 * b0 + a1 * b1 + a2 * b2

abs' :: Pos -> Int
abs' (Pos (a0, a1, a2)) = abs a0 + abs a1 + abs a2

type Face = (Pos, Pos)

parse :: String -> Set Pos
parse = Set.fromList . map ((\is -> Pos (is !! 0, is !! 1, is !! 2)) . map read . splitOn ",") . lines

dirs :: [Pos]
dirs = do
  dx <- [-1 .. 1]
  dy <- [-1 .. 1]
  dz <- [-1 .. 1]
  let p = Pos (dx, dy, dz)
  guard $ abs' p == 1
  return p

openFaces :: Set Pos -> Set Face
openFaces ps = Set.fromList . concatMap (\p -> zip (repeat p) (open p)) . Set.toList $ ps
  where
    open p = filter (flip Set.notMember ps) . map (.+ p) $ dirs

adjacent :: Set Face -> Face -> Set Face
adjacent fcs (p1, p2) = Set.fromList $ adirs
  where
    adirs =
      mapMaybe
        (\d -> find (`Set.member` fcs) [(p2 .+ d, p2), (p1 .+ d, p2 .+ d), (p1, p1 .+ d)])
        [d | d <- dirs, d .* (p2 .- p1) == 0]

go :: Face -> Set Face -> Set Face
go from faces = faces \\ go' from faces
  where
    adj = adjacent faces
    go' f d = Set.foldr go' (d \\ adj f) (d `Set.intersection` adj f)

part1 :: String -> Int
part1 = length . openFaces . parse

part2 :: String -> Int
part2 s = case Set.maxView faces of
  Just (a, _) -> length $ go a faces
  _ -> -1
  where
    faces = openFaces . parse $ s

run :: IO ()
run = do
  r <- req 18
  putStrLn "--- Day 18 ---"
  putStrLn . ((++) "part 1: ") . show $ part1 r
  putStrLn . ((++) "part 2: ") . show $ part2 r
