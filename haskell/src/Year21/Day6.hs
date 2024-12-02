{-# LANGUAGE OverloadedStrings #-}

module Year21.Day6 (main) where

import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import Control.Monad.IO.Class
import Network.HTTP.Req

reqDay :: Int -> IO BsResponse
reqDay d = runReq defaultHttpConfig $
                   req GET (https "adventofcode.com" /: "2021" /: "day" /: (T.pack.show) d /: "input")
                       NoReqBody bsResponse $ 
                       header "Cookie" "_ga=GA1.2.476278310.1638606484; _gid=GA1.2.1700296996.1638606484; session=53616c7465645f5f5cc00fdbaca34172effc09c45d732427581595ddacb7bdd720ebc395b0eed91d5da5328f2ba5ac46" 

readlines :: BsResponse -> [C.ByteString]
readlines r = C.lines $ responseBody r

parse :: [C.ByteString] -> [Int]
parse c =  map (read.C.unpack) $ C.split ','.head $ c

day :: [Int] -> [Int]
day (f:fs) = let (s,l:e) = splitAt 6 fs
             in s++(l+f):e++[f]

fill :: [Int] -> [Int]
fill fs = foldr atb [0,0,0,0,0,0,0,0,0] fs
  where atb = (\i a -> let (f,k:b) = splitAt i a 
                       in f++k+1:b)

main :: IO ()
main = do
  r <- reqDay 6
  let fishes = fill.parse.readlines $ r
  let days = iterate day fishes
  print $ sum $ days !! 256


