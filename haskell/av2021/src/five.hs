{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import Data.List.Split
import Numeric.LinearAlgebra.Data
import Control.Monad.IO.Class
import Network.HTTP.Req

reqDay :: Int -> IO BsResponse
reqDay d = runReq defaultHttpConfig $
                   req GET (https "adventofcode.com" /: "2021" /: "day" /: (T.pack.show) d /: "input")
                       NoReqBody bsResponse $ 
                       header "Cookie" "_ga=GA1.2.476278310.1638606484; _gid=GA1.2.1700296996.1638606484; session=53616c7465645f5f5cc00fdbaca34172effc09c45d732427581595ddacb7bdd720ebc395b0eed91d5da5328f2ba5ac46" 

readlines :: BsResponse -> [C.ByteString]
readlines r = C.lines $ responseBody r

rlines :: String -> [C.ByteString]
rlines = C.lines.C.pack 

type Position = (Int,Int)
type Line = (Position, Position)

parse :: [C.ByteString] -> [Line]
parse a =  map rvec $ strs a
  where rvec = \[a,b] -> (rpos a, rpos b)
        rpos = (\[a,b] -> (read a,read b)).splitOn "," 
        strs = map (splitOn " -> ".C.unpack)

getRange :: [Line] -> ((Int,Int),(Int,Int))
getRange = foldr cmpline ((0,0),(0,0))
  where cmpline =(\((a,b),(c,d)) (xr,yr) -> (cmpint c $ cmpint a xr, cmpint b $ cmpint d yr))
        cmpint  =(\p (mi,ma) -> (min mi p , max ma p))

zeros :: ((Int,Int),(Int,Int)) -> Matrix Z
zeros ((x1,x2),(y1,y2)) = ((x2-x1+1)><(y2-y1+1)) [0,0..]

drawline :: Line -> Matrix Z -> Matrix Z
drawline a z = accum z (+) $ zip (pixels a) (repeat 1)  

drawpic :: [Line] -> Matrix Z
drawpic l = foldr drawline (zeros.getRange $ l) l 

countpic :: Matrix Z -> Int
countpic z = foldr (\a b -> if a > 1 then b+1 else b) 0 $ toList $ flatten z

pixels :: Line -> [Position]
pixels ((x1,y1),(x2,y2))
  | x1 == x2  = zip (repeat x1) (rn (y1,y2))
  | y1 == y2  = zip (rn (x1,x2)) (repeat y1)
  | (x2-x1)*(y2-y1) > 0 = zip (rn (x1,x2)) (rn (y1,y2))
  | (x2-x1)*(y2-y1) < 0 = zip (rn (x1,x2)) (reverse $ rn (y1,y2))
  where rn = (\(a,b) -> if a < b then [a..b] else [b..a])  

main :: IO (Matrix Z, [Line])
main = do
  r <- reqDay 5
  --r <- readFile "test.csv"
  let lines' = parse.readlines $ r
  let z = drawpic lines'
  print $ tr z
  print $ countpic z
  return (z,lines')

