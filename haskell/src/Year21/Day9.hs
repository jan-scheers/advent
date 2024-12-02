{-# LANGUAGE OverloadedStrings #-}

module Year21.Day9 (main) where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import Control.Monad.IO.Class
import Network.HTTP.Req
import Data.Matrix
import Data.List (sort)
import Data.Set (Set, union, member, empty, singleton)
import qualified Data.Set as Set 
import Data.Maybe (catMaybes)



reqDay :: Int -> IO BsResponse
reqDay d = runReq defaultHttpConfig $
                   req GET (https "adventofcode.com" /: "2021" /: "day" /: (T.pack.show) d /: "input")
                       NoReqBody bsResponse $ 
                       header "Cookie" "_ga=GA1.2.476278310.1638606484; _gid=GA1.2.1700296996.1638606484; session=53616c7465645f5f5cc00fdbaca34172effc09c45d732427581595ddacb7bdd720ebc395b0eed91d5da5328f2ba5ac46" 

readlines :: BsResponse -> [C.ByteString]
readlines r = C.lines $ responseBody r

rlines :: String -> [C.ByteString]
rlines = C.lines.C.pack 

parse :: [C.ByteString] -> Matrix Int
parse cs = fromLists $ map pline cs
  where pline = \t -> map (\c -> read [c]) $ C.unpack t 

type Position = (Int,Int)

cross :: Matrix a -> Position-> [Position]
cross m (i,j) = verti++horiz
  where verti = catMaybes $ map (allow m) [(i,j-1),(i,j+1)] 
        horiz = catMaybes $ map (allow m) [(i-1,j),(i+1,j)] 

allow ::  Matrix a -> Position -> Maybe Position
allow m (i,j) = case safeGet i j m of 
                  Nothing -> Nothing
                  Just _ -> Just (i,j)


basin :: [Set Position] -> Matrix Int -> Position ->  Maybe (Set Position)
basin vi m p = if m ! p == 9 || any (member p) vi 
               then Nothing 
               else Just $ crawl empty m p

crawl :: Set Position -> Matrix Int -> Position -> Set Position
crawl vi m p = if m ! p == 9 || member p vi
               then empty
               else foldr (\n acc -> union acc $ crawl acc m n) 
                          (union vi $ singleton p) $ 
                          cross m p

basins :: Matrix Int -> [Set Position]
basins m = foldr (\p vi -> case basin vi m p of
                            Nothing -> vi
                            Just b -> b:vi) [] positions 
  where positions =  [(i,j) | i <- [1..(nrows m)], j <- [1..(ncols m)]]

main :: IO ()
main = do
  r <- reqDay 9
  let m = parse.readlines $ r
  -- r <- readFile "nine.csv"
  --let m = parse.rlines $ r
  print $ nrows m
  print $ ncols m
  let top3 =  take 3.reverse.sort.map length $ basins m
  print $ foldr1 (*) top3

