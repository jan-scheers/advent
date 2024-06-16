{-# LANGUAGE LambdaCase #-}

module Twentythree where

import Data.Maybe
import Data.Vector (Vector, cons, (//), (!))
import qualified Data.Vector as V
import Data.Map (Map)
import qualified Data.Map as M
import Data.List.Split

type Room = Vector (Maybe Char)
data Board = Board { hall  :: Room
                   , rooms :: Vector Room} deriving (Eq, Ord)
type BoardMap = Map (Int, Board) [Board]
type BoardState = ((Int, Board), [Board])
instance Show Board where
  show = pretty

parse :: String -> Board
parse s = Board hl rs
  where input = init.drop 1 $ lines s
        nrows = 4
        hl = (\i -> fromC $ head input !! i) <$> V.fromList (map ((+1).fhal 4) [0..6])
        rs = V.fromList $ map ((\i -> V.fromList.map (\s -> fromC (s !! i)) $ tail input).(+1).frow) [0..3] 
        fromC = \case '.' -> Nothing
                      c -> Just c

pretty :: Board -> String
pretty (Board vh vrs) = init.unlines $ concat [[t,hallS,room1],tail roomS,[b]]
  where hl = V.toList vh
        rs = map V.toList $ V.toList vrs
        t = replicate 13 '#'
        hallS = concat [ '#' : [toC.head $ hl]
                       , foldr (\jc acc -> toC jc:'.':acc) [] (take 4.drop 1 $ hl)
                       , (toC.last.init $ hl) : (toC.last $ hl) : ['#']]
        len = length $ head rs
        roomS = map (\i -> concat ["  ", foldr (\s acc -> '#':toC (s !! i):acc) ['#'] rs, "  "]) [0..len-1]
        room1 = concat ["##", take 9.drop 2 $ head roomS, "##"]
        toC = \case Nothing -> '.'
                    Just c -> c
        b = concat ["  ", replicate 9 '#', "  "]

frow r = 2+r*2

fhal nr h
  | h < 2 = h
  | h < 2+nr = 2*h - 1
  | otherwise = h + nr

moveDist :: Int -> Int -> Int
moveDist r h = abs (frow r - fhal 4 h)

moveCost :: Char -> Int -> Int -> Int -> Int
moveCost c r h ri = 10^(c2i c)*(moveDist r h + ri)
        
slice :: Int -> Int -> Vector a -> Vector a
slice rowInd j = if j < i then V.slice j (i-j) else V.slice i (j-i+1)
  where i = rowInd + 2

moveUp :: BoardState -> Int -> [BoardState]
moveUp ((cost, b), bs) i = if amphis == V.empty || all (== i2c i) amphis then []
  else map moveTo $ filter (canMoveUp (hall b) i) [0..6]
  where room = rooms b ! i
        amphis = V.catMaybes room
        j = length room - length amphis
        (c, ri') = popRoom (rooms b ! i) j 
        moveTo = \k -> (( cost + moveCost (fromJust c) i k (j+1)
                        , Board (hall b // [(k, c)]) (rooms b // [(i, ri')])
                        ), b:bs)

popRoom :: Room -> Int -> (Maybe Char, Room)
popRoom rm i = (rm ! i, rm // [(i, Nothing)])

canMoveUp :: Room -> Int -> Int -> Bool
canMoveUp hl ri j = V.null.V.catMaybes $ slice ri j hl

c2i :: Char -> Int
c2i = flip (-) 65. fromEnum

i2c :: Int -> Char
i2c = toEnum.(+) 65

moveDown :: BoardState -> [BoardState]
moveDown ((c, b), bs) = map (\k -> (pushRoom (c, b) k, b:bs)) $ filter (canMoveDown b) [0..6]


pushRoom :: (Int, Board) -> Int -> (Int, Board)
pushRoom (cost, b) k =  (cost', Board (hall b // [(k, Nothing)]) (rooms b // [(i, r')]))
  where c = fromJust $ hall b ! k
        i = c2i c
        (n, js) = V.break isJust (rooms b ! i)
        r' = V.init n V.++ Just c `cons` js
        cost' = cost + moveCost c i k (length n)
  
canMoveDown :: Board -> Int -> Bool
canMoveDown b k = case hall b ! k of
  Nothing -> False
  Just c -> emptyHall c && canDrop c
  where emptyHall = \c -> V.singleton c == V.catMaybes (slice (c2i c) k (hall b))
        canDrop = \c -> not.any (/=c).V.catMaybes $ rooms b ! c2i c

cheap :: ((Int, Int), BoardMap) -> ((Int, Int), BoardMap)
cheap ((mt, ct), hp) = if isFinished hd then ((mt, ct), uncurry M.singleton hd)
  else cheap ((mt + length moves, ct + 1), foldr (uncurry M.insert) tl moves)
  where (hd, tl) = fromJust $ M.minViewWithKey hp
        moves = concat (moveDown hd: map (moveUp hd) [0..3])

isFinished :: BoardState -> Bool
isFinished h = null (concatMap (moveUp h) [0..3]) && null (V.catMaybes.hall.snd.fst $ h) 

tile :: Show a => Int -> [a] -> String
tile i b = concatMap showTile bi
  where bi = chunksOf i b
        len = length.lines.show.head $ b
        showTile = \ti -> unlines $ map (\i -> drop 2 $ concatMap (("  " ++).(!! i).lines.show) ti) [0..len-1]

run :: String -> IO ()
run r = do
  let b = parse r
  let ((nmoves,nct), ch) = cheap ((0,0), M.singleton (0, b) [])
  let ((cost, final), path) = head.M.toList $ ch
  print (nmoves, nct)
  print cost
  putStr (tile 5.reverse $ final:path)

