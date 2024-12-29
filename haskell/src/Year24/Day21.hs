{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Year24.Day21 (main) where

import Control.Monad (guard)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.PSQueue (Binding ((:->)))
import qualified Data.PSQueue as PSQ
import qualified Data.Text as T
import qualified Data.Vector as V
import Distribution.Compat.Binary (Binary (put))
import Distribution.Compat.Graph (neighbors)
import Lib (Pos, charToDir, delta, deltaToChar, norm_1, plus, requestDay)
import Matrix ((!))
import qualified Matrix as Mat

_test :: IO T.Text
_test = T.pack <$> readFile "./src/Year24/Day21.txt"

parse :: T.Text -> [String]
parse = map T.unpack . T.lines

main :: IO ()
main = do
  putStrLn "Day 21"
  codes <- parse <$> _test
  printNumbers
  printArrows
  let c = codes !! 3
  putStrLn . unlines $ [c, goNumbers c, goArrows . goNumbers $ c, goArrows . goArrows . goNumbers $ c]

type PathMap = Map.Map (Char, Char) String

type Field = Mat.Matrix Char

partOne :: [String] -> [(Int, Int)]
partOne = map line
  where
    calc = length . goArrows . goArrows . goNumbers
    line l = (calc l, (read (init l) :: Int))

goNumbers :: String -> String
goNumbers s = concatMap (\p -> numbersOld Map.! p ++ "A") $ zip ('A' : s) s

goArrows :: String -> String
goArrows s = concatMap (\p -> arrowsOld Map.! p ++ "A") $ zip ('A' : s) s

printNumbers :: IO ()
printNumbers = putStrLn . Mat.prettyMatrix . Mat.fromLists $ ["789", "456", "123", " 0A"]

numbersOld :: PathMap
numbersOld = processKeypad $ Mat.fromLists ["789", "456", "123", " 0A"]

printArrows :: IO ()
printArrows = putStrLn . Mat.prettyMatrix . Mat.fromLists $ [" ^A", "<v>"]

arrowsOld :: PathMap
arrowsOld = processKeypad $ Mat.fromLists [" ^A", "<v>"]

processKeypad :: Field -> PathMap
processKeypad field = Mat.ifoldr forChar Map.empty field
  where
    forChar pos c m = if c == ' ' then m else Mat.ifoldr insertPath m field
      where
        route = dijkstra field pos
        insertPath pos' c' = Map.insert (c, c') (follow route pos')

type Best = Map.Map (State, String) Int

dijkstra :: State -> Char -> String
dijkstra start target = fromJust $ go (PSQ.singleton (start, "") 0) Map.empty
  where
    go :: PSQ.PSQ (State, String) Int -> Best -> Maybe String
    go queue best = PSQ.minView queue >>= go' best
    go' best (curr@(state, path) :-> cost, queue) =
      if didFind pressed then Just ('A' : path) else go queue' best'
      where
        best' = Map.insert curr cost best
        queue' =
          foldr
            (\n q -> PSQ.insertWith min n (cost + 1) q)
            queue
            (filter (`Map.notMember` best) $ getPress pressed ++ neigbors)

        neigbors = map (\(c, s) -> (s, c : path)) $ next state
        pressed = press state

        didFind (Pressed s) = numbers ! V.last s == target
        didFind _ = False

        getPress (Valid s) = [(s, 'A' : path)]
        getPress _ = []

isValid :: Field -> Pos -> Bool
isValid field p = Mat.inRange field p && field Mat.! p /= ' '

numbers :: Field
numbers = Mat.fromLists ["789", "456", "123", " 0A"]

arrows :: Field
arrows = Mat.fromLists [" ^A", "<v>"]

type State = V.Vector Pos

next :: State -> [(Char, State)]
next rs = do
  let head' = V.head rs
  p <- plus head'
  guard $ isValid numbers p
  return (deltaToChar (p - head'), V.cons p (V.init rs))

data Found
  = Pressed {_getFound :: State}
  | Valid {_getFound :: State}
  | Invalid

press :: State -> Found
press rs = case V.findIndex (\p -> arrows ! p /= 'A') (V.init rs) of
  Nothing -> Pressed rs
  Just i ->
    let moveNext = delta . charToDir $ arrows ! (rs V.! i)
     in if isValid (if i >= V.length rs - 1 then numbers else arrows) (rs V.! i + moveNext)
          then Valid (rs V.// [(i, rs V.! i + moveNext)])
          else Invalid
