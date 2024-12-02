{-# LANGUAGE OverloadedStrings #-}

module Year21.Day11 (main) where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import Control.Monad.IO.Class
import Network.HTTP.Req
import Data.Matrix
import Data.Maybe (mapMaybe)

reqDay :: Int -> IO BsResponse
reqDay d = runReq defaultHttpConfig $
                   req GET (https "adventofcode.com" /: "2021" /: "day" /: (T.pack.show) d /: "input")
                       NoReqBody bsResponse $ 
                       header "Cookie" "_ga=GA1.2.476278310.1638606484; _gid=GA1.2.1700296996.1638606484; session=53616c7465645f5f5cc00fdbaca34172effc09c45d732427581595ddacb7bdd720ebc395b0eed91d5da5328f2ba5ac46" 

readlines :: BsResponse -> [C.ByteString]
readlines r = C.lines $ responseBody r

readlines' :: String -> [C.ByteString]
readlines' = C.lines.C.pack 

fromBytes :: [C.ByteString] -> Matrix Int
fromBytes cs = fromLists $ map pline cs
  where pline = map (\c -> read [c]).C.unpack 

type Position = (Int,Int)

square :: Matrix a -> Position-> [Position]
square m (i,j) = mapMaybe (allow m) eight
  where eight = filter (\(a,b) -> not $ a == i && b == j) nine
        nine = [(a,b) | a <- [(i-1)..(i+1)], b <- [(j-1)..(j+1)]]

allow ::  Matrix a -> Position -> Maybe Position
allow m (i,j) = case safeGet i j m of 
                  Nothing -> Nothing
                  Just _ -> Just (i,j)

flash :: Position -> Matrix Int -> Matrix Int
flash pos mat = case uncurry unsafeGet pos mat of
                0 -> mat
                a -> if a < 9 then unsafeSet (a+1) pos mat
                              else resolve (unsafeSet 0 pos mat) pos
  where resolve = \m p -> foldr flash m $ square m p

trigger :: Position -> Matrix Int -> Matrix Int
trigger pos mat = let a = uncurry unsafeGet pos mat in 
                  if a > 9 then flash pos mat else mat

step :: Matrix Int -> Matrix Int
step m = foldr trigger inc pos
  where inc = fmap (+1) m
        pos = [(a,b) | a <- [1..nrows m], b <- [1..ncols m]]

count :: Matrix Int -> Int
count = foldr (\i acc -> if i == 0 then acc+1 else acc) 0

main :: IO ()
main = do
  --r <- readFile "eleven.csv"
  r <- reqDay 11
  let m = fromBytes.readlines $ r
  let steps = tail $ iterate step m
  print.sum $ map count $ take 100 steps
  let sync = length $ takeWhile (< nrows m * ncols m) $ map count steps
  print $ steps !! (sync-1)
  print $ steps !! sync
  print $ steps !! (sync+1)
  print (sync+1)

