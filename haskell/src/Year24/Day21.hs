module Year24.Day21 (main) where

import Control.Monad (guard)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Vector as V
import Lib (Pos, charToDir, delta, requestDay)
import Matrix ((!))
import qualified Matrix as Mat

_test :: IO T.Text
_test = T.pack <$> readFile "./src/Year24/Day21.txt"

parse :: T.Text -> [String]
parse = map T.unpack . T.lines

main :: IO ()
main = do
  putStrLn "Day 21"
  codes <- parse <$> requestDay 21
  putStrLn . Mat.prettyMatrix $ numpad
  putStrLn . Mat.prettyMatrix $ arrows
  print $ partOne codes
  print $ partTwo codes

partOne :: [String] -> Int
partOne = sum . map calc
  where
    calc code = read (init code) * enterCode 2 code

partTwo :: [String] -> Int
partTwo = sum . map calc
  where
    calc code = read (init code) * enterCode 25 code

enterCode :: Int -> String -> Int
enterCode n code = sum $ zipWith cost ('A' : code) code
  where
    graph = iterate (nextMap arrows) graph0 !! n
    cost curr next = bfs numpad graph (findChar curr numpad) Map.! findChar next numpad

type Field = Mat.Matrix Char

findChar :: Char -> Field -> Pos
findChar c = fromJust . Mat.findIndex (== c)

isValid :: Field -> Pos -> Bool
isValid field p = Mat.inRange field p && field Mat.! p /= ' '

numpad :: Field
numpad = Mat.fromLists ["789", "456", "123", " 0A"]

arrows :: Field
arrows = Mat.fromLists [" ^A", "<v>"]

arrowPos :: [((Int, Int), Char)]
arrowPos = filter (\(_, c) -> c /= ' ') $ Mat.toList . Mat.indexed $ arrows

posA :: Pos
posA = findChar 'A' arrows

graph0 :: CostMap
graph0 =
  let m = Map.fromList $ [(p, 1) | p <- map fst arrowPos]
   in Map.fromList [(p, m) | p <- map fst arrowPos]

type Robots = V.Vector Pos

_simulate :: Int -> String -> String
_simulate n input = unlines $ (' ' : input) : map row [0 .. n]
  where
    fields = V.fromList $ replicate n arrows ++ [numpad]
    start = V.map (fromJust . Mat.findIndex (== 'A')) fields
    state = start : sim start input
    row i = map (\col -> (fields V.! i) ! (col V.! i)) state
    sim _ [] = []
    sim robots (c : cs) =
      let robots' =
            if c == 'A'
              then case _press robots of
                Just (rs', _) -> rs'
                Nothing -> robots
              else V.cons (V.head robots + delta (charToDir c)) (V.tail robots)
       in robots' : sim robots' cs

_press :: Robots -> Maybe (Robots, Bool)
_press rs = case V.findIndex (\p -> arrows ! p /= 'A') (V.init rs) of
  Nothing -> Just (rs, True)
  Just i ->
    if isValid field moved
      then Just (rs V.// [(i + 1, moved)], False)
      else Nothing
    where
      arrow = arrows ! (rs V.! i)
      field = if i >= (V.length rs - 2) then numpad else arrows
      moved = (rs V.! (i + 1)) + (delta . charToDir $ arrow)

type CostMap = Map.Map Pos (Map.Map Pos Int)

nextMap :: Field -> CostMap -> CostMap
nextMap field graph = Map.fromList $ [(p, bfs field graph p) | p <- map fst arrowPos]

bfs :: Field -> CostMap -> Pos -> Map.Map Pos Int
bfs field graph start =
  Map.foldrWithKey (insertMin . snd) Map.empty $
    go Map.empty [((posA, start), 0)]
  where
    getPath a b = graph Map.! a Map.! b
    insertMin key cost acc = case maybe LT (compare cost) (Map.lookup key acc) of
      LT -> Map.insert key cost acc
      _ -> acc

    go :: Map.Map (Pos, Pos) Int -> [((Pos, Pos), Int)] -> Map.Map (Pos, Pos) Int
    go best [] = best
    go best ((curr@(mover, rider), cost) : queue) =
      case maybe LT (compare cost') (Map.lookup curr best) of
        LT -> go (Map.insert curr cost' best) (queue ++ moveRider)
        _ -> go best queue
      where
        cost' = cost + getPath mover posA
        moveRider = do
          (mover', c) <- arrowPos
          guard $ c /= 'A'
          let rider' = rider + delta (charToDir c)
          guard $ isValid field rider'
          return ((mover', rider'), cost + getPath mover mover')
