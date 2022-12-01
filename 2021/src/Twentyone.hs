module Twentyone where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Bifunctor

data Player = Player { pos :: Int, score ::Int } deriving (Eq, Ord)
type State = (Player, Player)
type Universe = Map State Int

instance Show Player where
  show p = "(p: " ++ show (pos p) ++ ", sc: " ++ show (score p) ++ ")"

play :: Int -> Universe -> IO Universe
play i u = if M.null playing then return done else print (total u, total done) >> play (1-i) u'
  where (playing, done) = split u
        u' = playply i done playing

split :: Universe -> (Universe, Universe)
split = M.partitionWithKey (\k _ -> isPlaying k)

isPlaying :: State -> Bool
isPlaying (p1, p2) = score p1 < 21 && score p2 < 21

playply :: Int -> Universe -> Universe -> Universe
playply i = M.foldrWithKey (ply i)

ply :: Int -> State -> Int -> Universe -> Universe
ply k s cnt u = foldr (\k -> M.insertWith (+) k cnt) u rolls
  where rolls = map (\i -> (if k == 0 then first else second) (throw i) s ) 
                    [(i,j,k) | i <- [1..3], j <- [1..3], k <- [1..3]]
  

throw :: (Int, Int, Int) -> Player -> Player
throw (i, j, k) p = Player p' (min 21 $ score p + p' + 1)
  where p' = (pos p + i + j + k) `mod` 10

count :: Universe -> (Int, Int)
count = M.foldrWithKey winner (0,0)
  where winner = \s i acc -> if score (fst s) == 21 then first (+i) acc 
                             else if score (snd s) == 21 then second (+i) acc
                             else acc

total :: Universe -> Int
total = M.foldr (+) 0
 
start :: Universe
start = M.singleton (Player 8 0, Player 5 0) 1
test :: Universe
test  = M.singleton (Player 3 0, Player 7 0) 1

{-
play :: GameState -> GameState
play (d, (p1, p2)) = if snd p1' >= 1000 then (d', (p1', p2))
  else if snd p2' >= 1000 then (d'', (p1', p2'))
    else play (d'', (p1', p2'))
  where (p1',d') = throw (p1, d)
        (p2',d'') = throw (p2, d')
  
throw :: (Player, Die) -> (Player, Die)
throw ((p, score), d) = ((p', score'), d')
  where p' = (p + f d) `mod` 10
        d' = d + 1
        score' = score + p' + 1
  
-}
