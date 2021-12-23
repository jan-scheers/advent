module Nineteen where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Data.List (permutations, sort, nub)
import Data.Map (Map, assocs)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Control.Applicative
import Prelude hiding ((<>))

type Beacon = Vector I
type Orientation = Matrix I
type Scanner = [Beacon]
type Direction = (Orientation, Vector I)

split :: (a -> Bool) -> [a] -> [[a]]
split f a = case break f a of 
  (a', [])  -> [a']
  (a', _:as)  -> a':split f as 

parseInput :: String -> [Scanner] 
parseInput s = map readScanner scanners
  where scanners = init.split null.split (== '\n') $ s
        readScanner = map readBeacon.tail
        readBeacon = \s -> read ("[" ++ s ++ "]")
  
turns :: [[Int]]
turns = b.b.b $ [[]]
  where b =((:) <$> [-1,1] <*>)

orientations :: [Matrix I]
orientations = nub $ 
               turns >>= \t -> 
               map (take 2) (permutations [0, 1, 2]) >>= \s -> 
               let [v1,v2] = toRows $ diag (idxs t) ?? (Pos (idxs s), All) in
               return (fromRows [v1,v2, cross v1 v2])

bucket :: Ord v => [v] -> Map v Int
bucket = foldr (\k m -> Map.insertWith (+) k 1 m) Map.empty

maxByVal :: Ord b => Map a b -> (a, b)
maxByVal = foldr1 (\(k, v) ma -> if v > snd ma then (k, v) else ma).assocs

match :: Scanner -> Scanner -> Maybe Direction
match s0 s1 = case mapMaybe filterCand (candPairs s0 s1) of 
  [] -> Nothing
  pass -> Just (head pass) 

candPairs :: Scanner -> Scanner -> [(Orientation, [(Beacon, Beacon)])]
candPairs s0 s1 = zip orientations $ map (try sect) orientations
  where sect = Map.intersectionWithKey (\k a b -> (a,b)) (distMap s0) (distMap s1) 
        try  = \s o -> nub $ Map.foldr (\(a,b) acc -> matchPair a (both (o #>) b) ++ acc) [] s
        both = \f (a,b) -> (f a, f b)

distMap :: Scanner -> Map R (Beacon, Beacon)
distMap s = Map.fromList $ map (\(a,b) -> (norm_2 (a-b), (a,b))) pairs
  where pairs = [(a,b) | a <- s, b <- s, a < b]

matchPair :: (Beacon, Beacon) -> (Beacon, Beacon) -> [(Beacon, Beacon)]
matchPair (a,b) (c,d) 
  | straight = [(a, c), (b, d)]
  | crossed = [(a, d), (b, c)]
  | otherwise = []
  where straight = a-c == b-d
        crossed  = a-d == b-c

filterCand :: (Orientation, [(Beacon, Beacon)]) -> Maybe Direction
filterCand (_,[]) = Nothing
filterCand (o,pairs) = let (v, c) = maxByVal.bucket.map (uncurry (-)) $ pairs in
    if c < 12 then Nothing else Just (o,v)

scanDir :: [Scanner] -> Int -> [Maybe Direction] -> [Maybe Direction]
scanDir s k prev = foldr (\i n -> if should i then scanDir s i n else n) next [0..length s-1]
  where curr = map (match $ s !! k) s
        next = zipWith (merge $ prev !! k) prev curr
        should = \i -> isNothing (prev !! i) && isJust (next !! i)

merge :: Maybe Direction -> Maybe Direction -> Maybe Direction -> Maybe Direction
merge _ (Just a) _ = Just a
merge _ Nothing Nothing = Nothing
merge rt Nothing (Just (r', d')) = let (r, d) = fromJust rt 
                                   in Just (r <> r', apply (r, d) d')

apply :: Direction -> Vector I -> Vector I
apply (rMat, d) v = d + rMat #> v

run :: String -> IO ()
run r = do
  let scanners = parseInput r
  let start = match (head scanners) (head scanners) : repeat Nothing
  let dirs = catMaybes $ scanDir scanners 0 start
  print dirs
  print.length.nub.concat $ zipWith (map.apply) dirs scanners
  print.maximum $ map (\(_, d) -> norm_1 d) dirs 
