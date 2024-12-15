module Year24.Day9 (main) where

import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import qualified Data.Vector as V
import Lib (requestDay)

_test :: IO T.Text
_test = readFile "./src/Year24/Day9.txt" <&> T.pack

parse :: T.Text -> V.Vector (Maybe Int)
parse = V.fromList . f 0 . T.unpack . T.strip
  where
    f _ "" = []
    f i (x : xs) = replicate (read [x] :: Int) (Just i) ++ g i xs
    g _ "" = []
    g i (x : xs) = replicate (read [x] :: Int) Nothing ++ f (i + 1) xs

_pretty :: V.Vector (Maybe Int) -> [Char]
_pretty = V.toList . V.map (maybe '.' (head . show))

partOne :: V.Vector (Maybe Int) -> Int
partOne v = sum . V.imap (\i x -> i * fromMaybe 0 x) $ swap 0 (V.length v - 1) v

swap :: Int -> Int -> V.Vector (Maybe a) -> V.Vector (Maybe a)
swap i j v
  | i >= j = v
  | otherwise = case (v V.! i, v V.! j) of
      (Nothing, Just y) -> swap i (j - 1) (v V.// [(i, Just y), (j, Nothing)])
      (Just _, Just _) -> swap (i + 1) j v
      (_, Nothing) -> swap i (j - 1) v

partTwo :: V.Vector (Maybe Int) -> Int
partTwo v = sum . V.imap (\i x -> i * fromMaybe 0 x) $ shake (V.length v) v

shake :: Int -> V.Vector (Maybe Int) -> V.Vector (Maybe Int)
shake i v = case nextSlice i v of
  Nothing -> v
  Just slice@(j, _) -> shake j (fit slice v)

nextSlice :: Int -> V.Vector (Maybe Int) -> Maybe (Int, Int)
nextSlice j v = if not (null y) then Just (last y, length y) else Nothing
  where
    x = dropWhile (isNothing . (V.!) v) . reverse $ [0 .. j - 1]
    val = v V.! head x
    y = takeWhile (\i -> v V.! i == val) x

fit :: (Int, Int) -> V.Vector (Maybe a) -> V.Vector (Maybe a)
fit (j, n) v = case emptySlice of
  Nothing -> v
  Just i -> v V.// ([(k, v V.! j) | k <- [i .. (i + n - 1)]] ++ [(k, Nothing) | k <- [j .. (j + n - 1)]])
  where
    emptySlice = fst . V.ifoldl findSlice (Nothing, 0) $ V.take j v
    findSlice slice@(found, c) i' x
      | isJust found = slice
      | isNothing x = (if c + 1 == n then Just (i' - c) else Nothing, c + 1)
      | otherwise = (Nothing, 0)

main :: IO ()
main = do
  print "Day 9"
  input <- requestDay 9 <&> parse
  print $ partOne input
  print $ partTwo input
