module Year22.Day17.Run (run) where
import Year22.Lib (req)

import Control.Monad ( foldM )
import Data.Matrix (Matrix, (!), (<->), nrows, mapPos)
import qualified Data.Matrix as Mat
import qualified Data.Vector as Vec
import Data.List (findIndex)
import Data.Maybe (fromMaybe)

data PrettyTetris = PrettyTetris (Matrix Int)
instance Show PrettyTetris where
    show (PrettyTetris m) = unlines.map (\s -> '|':s++"|").Mat.toLists.mapPos (\(y, x) _ -> if m ! (nrows m - y + 1, x) > 0 then '\x2588' else ' ') $ m

type Pos = (Int, Int)

(.+) :: Pos -> Pos -> Pos
(.+) (a, b) (c, d) = (a + c, b + d)

type Shape = [Pos]
shapes :: [Shape]
shapes = [
    [(1,x) | x <- [1..4]],
    (1,2):(3,2):[(2,x) | x <- [1..3]],
    (2,3):(3,3):[(1,x) | x <- [1..3]],
    [(y,1) | y <- [1..4]],
    [(y,x) | y <- [1..2], x <- [1..2]]
    ]

data State = State {
    shape :: Int, 
    depth :: Int, 
    tower :: Matrix Int 
    }
instance Show State where
    show (State _ _ tw) = show (PrettyTetris tw)
instance Eq State where
    (==) a b = shape a == shape b && depth a == depth b && towerTail a == towerTail b
        where towerTail e = (\v -> Vec.drop (Vec.length v - 7 * (depth e + 4)) v).Mat.getMatrixAsVector.tower $ e


blocks :: String -> [State]
blocks input = blocks' (zip [0..] shapes) (Mat.fromLists [replicate 7 1], init input)

blocks' :: [(Int, Shape)] -> (Matrix Int, String) -> [State]
blocks' ((i,s):sh) (curr, jets) = state: blocks' (sh ++ [(i,s)]) (nextTower, nextJets)
    where padded = if top curr >= 7 then curr else curr <-> Mat.zero (7 - top curr) 7
          (nextTower, nextJets, d) = dropBlock padded jets s
          state = State i d nextTower
blocks' [] _ = []

top :: Matrix Int -> Int
top tower = fromMaybe (-1).findIndex (>0).fmap (\i -> sum $ Mat.getRow i tower) $ [n, n-1 ..]
    where n = nrows tower

height :: Matrix Int -> Int
height tower = nrows tower - top tower

dropBlock :: Matrix Int -> String -> Shape -> (Matrix Int, String, Int)
dropBlock tower jets shape = dropBlock' (tower, jets, 0) ((nrows tower - 4, 2), shape)

dropBlock' :: (Matrix Int, String, Int) -> (Pos, Shape) -> (Matrix Int, String, Int)
dropBlock' (tower, j:js, k) ((y0, x0), shape) = if test tower ((y, x), shape)
    then dropBlock' (tower, jets, k + 1) ((y, x), shape)
    else (paste (y0, x) tower shape, jets, k + 1)
    where xt = if j == '<' then x0 - 1 else x0 + 1
          x = if test tower ((y0, xt), shape) then xt else x0
          y = y0 - 1
          jets = js ++ [j]
dropBlock' a _ = a

paste :: Pos -> Matrix Int -> Shape -> Matrix Int
paste pos = foldr (\p ->  Mat.setElem 1 (p .+ pos)) 

test :: Matrix Int -> (Pos, Shape) -> Bool
test tower (delta, shape) = case foldM (\b pos -> uncurry Mat.safeGet (pos .+ delta) tower >>= \v -> Just (b && v == 0)) True shape of
    Just b -> b
    _ -> False

period :: [State] -> (Int, [State])
period s = period' ([], s)

period' :: ([State], [State]) -> (Int, [State])
period' (tip, r:rest) = case prev >>= tryReturn of
    Just result -> result
    Nothing -> period' (tip ++ [r], rest)
    where prev = findIndex (== r) $ reverse tip
          tryReturn i = if all (uncurry (==)) $ zip a b then Just (offset, tip !! offset:a) else Nothing
            where a = reverse.(r:).take i.reverse $ tip
                  b = take (i + 1) rest
                  offset = length tip - i - 1
period' _ = (-1, [])

calc :: Integer -> (Int, [State]) -> Integer
calc n (offset, towerCycle) = offsetHeight + q * last heights + (heights !! fromIntegral r)
    where offsetHeight = toInteger.height.tower.head $ towerCycle
          heights = map ((\h -> h - offsetHeight).toInteger.height.tower) towerCycle
          cycleLength = toInteger $ length heights - 1
          p = n - toInteger offset
          (q, r) = quotRem p cycleLength

          
part1 :: String -> Integer
part1  = (\a -> a - 1).calc 2021.period.blocks

part2 :: String -> Integer
part2  = (\a -> a - 1).calc 999999999999.period.blocks

run :: IO ()
run = do
    r <- req 17
    putStrLn "--- Day 17 ---"
    putStrLn.((++) "part 1: ").show $ part1 r
    putStrLn.((++) "part 2: ").show $ part2 r