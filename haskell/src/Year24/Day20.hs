module Year24.Day20 (main) where

import Control.Monad (guard)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.PSQueue (Binding ((:->)))
import qualified Data.PSQueue as PSQ
import qualified Data.Text as T
import Lib (Pos, norm_1, plus, requestDay)
import qualified Matrix as Mat

_test :: IO T.Text
_test = T.pack <$> readFile "./src/Year24/Day20.txt"

main :: IO ()
main = do
  putStrLn "Day 20"
  field <- parse <$> requestDay 20
  putStrLn . Mat.pretty $ field
  let end = dijkstra field (findEnd field)
  print . length $ cheats (100, 2) end
  print . length $ cheats (100, 20) end

type Field = Mat.Matrix Char

parse :: T.Text -> Field
parse = Mat.fromLists . map T.unpack . T.lines

findEnd :: Mat.Matrix Char -> (Int, Int)
findEnd = fromJust . Mat.findIndex (== 'E')

type Best = Map.Map Pos Int

cheats :: (Int, Int) -> Best -> [Int]
cheats (cut, maxDist) end =
  let reachable = Map.keys end
   in do
        p <- reachable
        p' <- reachable
        let dist = norm_1 (p - p')
        guard $ 0 < dist && dist <= maxDist
        let delta = end Map.! p - end Map.! p' - dist
        guard $ cut <= delta
        return delta

dijkstra :: Field -> Pos -> Best
dijkstra field start = go (PSQ.singleton start 0) Map.empty
  where
    go :: PSQ.PSQ Pos Int -> Best -> Best
    go queue best = case PSQ.findMin queue of
      Nothing -> best
      Just (pos :-> cost) -> go queue' best'
        where
          best' = Map.insert pos cost best
          queue' =
            foldr
              (\p q -> PSQ.insertWith min p (cost + 1) q)
              (PSQ.deleteMin queue)
              (filter (`Map.notMember` best) . filter isValid $ plus pos)

    isValid pos = field Mat.! pos /= '#'
