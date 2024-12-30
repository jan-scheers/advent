{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Year24.Day21 (main) where

import Control.Monad (guard)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.PSQueue (Binding ((:->)))
import qualified Data.PSQueue as PSQ
import qualified Data.Text as T
import qualified Data.Vector as V
import Lib (Pos, charToDir, delta, deltaToChar, plus, requestDay)
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
  printKeypad
  printArrows
  print $ partOne codes
  putStrLn $ enterCode 25 "0"

partOne :: [String] -> Int
partOne = sum . map calc
  where
    calc code = read (init code) * length (enterCode 2 code)

enterCode :: Int -> String -> String
enterCode n = reverse . go start
  where
    start = V.fromList $ map (fromJust . Mat.findIndex (== 'A')) $ replicate n arrows ++ [keypad]
    go _ [] = ""
    go rs (c : cs) = let (robots', path) = nextDigit rs c in go robots' cs ++ path

type Field = Mat.Matrix Char

printKeypad :: IO ()
printKeypad = putStrLn . Mat.prettyMatrix $ keypad

printArrows :: IO ()
printArrows = putStrLn . Mat.prettyMatrix $ arrows

type Robots = V.Vector Pos

type RobotPaths = Map.Map String Robots

nextDigit :: Robots -> Char -> (Robots, String)
nextDigit start target = fromJust $ go (PSQ.singleton (start, "") 0) Map.empty
  where
    go :: PSQ.PSQ (Robots, String) Int -> RobotPaths -> Maybe (Robots, String)
    go queue best = case PSQ.minView queue of
      Nothing -> Nothing
      Just (curr@(rs, path) :-> _, queue') -> case moveAndPress curr of
        (Just key@(rs', _), options) ->
          if keypad ! V.last rs' == target
            then Just key
            else explore options
        (Nothing, options) -> explore options
        where
          explore options = go (enqueue options) (Map.insert path rs best)
          enqueue next =
            foldr
              (\n@(_, path') q -> PSQ.insertWith min n (length path') q)
              queue'
              (filter (flip Map.notMember best . snd) next)

isValid :: Field -> Pos -> Bool
isValid field p = Mat.inRange field p && field Mat.! p /= ' '

keypad :: Field
keypad = Mat.fromLists ["789", "456", "123", " 0A"]

arrows :: Field
arrows = Mat.fromLists [" ^A", "<v>"]

arrowGraph :: PathMap
arrowGraph = pathMap arrows

moveAndPress :: (Robots, String) -> (Maybe (Robots, String), [(Robots, String)])
moveAndPress (rs, path) = foldr doPress (Nothing, []) possibleMoves
  where
    (zero, rest) = (V.head rs, V.tail rs)
    possibleMoves = Map.toList $ arrowGraph Map.! zero
    doPress (next, path') acc@(key, search) = case press (V.cons next rest) of
      Nothing -> acc
      Just (rs', True) -> (Just (rs', 'A' : path' ++ path), search)
      Just (rs', False) -> (key, (rs', 'A' : path' ++ path) : search)

press :: Robots -> Maybe (Robots, Bool)
press rs = case V.findIndex (\p -> arrows ! p /= 'A') (V.init rs) of
  Nothing -> Just (rs, True)
  Just i ->
    if isValid field moved
      then Just (rs V.// [(i + 1, moved)], False)
      else Nothing
    where
      arrow = arrows ! (rs V.! i)
      field = if i >= (V.length rs - 2) then keypad else arrows
      moved = (rs V.! (i + 1)) + (delta . charToDir $ arrow)

dijkstra :: Field -> Pos -> Map.Map Pos String
dijkstra field start = go (PSQ.singleton (start, "") 0) Map.empty
  where
    go :: PSQ.PSQ (Pos, String) Int -> Map.Map Pos String -> Map.Map Pos String
    go queue best = case PSQ.minView queue of
      Nothing -> best
      Just ((curr, path) :-> cost, queue') -> go enqueue best'
        where
          best' = Map.insert curr path best
          enqueue =
            foldr
              (\p q -> PSQ.insertWith min p (cost + 1) q)
              queue'
              (filter (flip Map.notMember best . fst) next)
          next = do
            p <- plus curr
            guard $ isValid field p
            return (p, deltaToChar (p - curr) : path)

type PathMap = Map.Map Pos (Map.Map Pos String)

pathMap :: Field -> PathMap
pathMap field = Map.fromList $ do
  (p, c) <- Mat.toList . Mat.indexed $ field
  guard $ c /= ' '
  return (p, dijkstra field p)


dijkstra' :: Field -> PathMap -> Pos -> Map.Map Pos String
dijkstra' field graph start = go (PSQ.singleton (start, "") 0) Map.empty
    go :: PSQ.PSQ (Pos, String) Int -> Map.Map Pos String -> Map.Map Pos String
    go queue best = case PSQ.minView queue of
      Nothing -> best
      Just ((curr, path) :-> cost, queue') -> go enqueue best'
        where
          best' = Map.insert curr path best
          enqueue =
            foldr
              (\p q -> PSQ.insertWith min p (cost + 1) q)
              queue'
              (filter (flip Map.notMember best . fst) next)
          next = do
            p <- plus curr
            guard $ isValid field p
            return (p, deltaToChar (p - curr) : path)