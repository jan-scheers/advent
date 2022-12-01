{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import Control.Monad.IO.Class
import Network.HTTP.Req
import Data.Map (Map, (!), member, insertWith, elems, union)
import qualified Data.Map as Map
import Data.Set ((\\))
import Data.List (sort)

reqDay :: Int -> IO C.ByteString
reqDay d = fmap responseBody $ runReq defaultHttpConfig $
                   req GET (https "adventofcode.com" /: "2021" /: "day" /: (T.pack.show) d /: "input")
                       NoReqBody bsResponse $ 
                       header "Cookie" "_ga=GA1.2.476278310.1638606484; _gid=GA1.2.1700296996.1638606484; session=53616c7465645f5f5cc00fdbaca34172effc09c45d732427581595ddacb7bdd720ebc395b0eed91d5da5328f2ba5ac46" 

type Pair = (Char, Char)

build :: C.ByteString -> Map Pair Char -> Map Pair Char
build bs = Map.insert (C.head bs, C.head.C.tail $ bs) (C.last bs)

rules :: C.ByteString -> Map Pair Char
rules bs = foldr build Map.empty ps
  where ps = tail.dropWhile (not.C.null).C.lines $ bs

start :: C.ByteString -> Map Pair Int
start bs = let pairs = zip s (tail s) in foldr inc Map.empty pairs
  where s = C.unpack.head $ C.lines bs
        inc = \c -> insertWith (+) c 1

step :: Map Pair Char -> Map Pair Int -> Map Pair Int
step rl st = Map.foldrWithKey (apply st) Map.empty rl

apply :: Map Pair Int -> Pair -> Char -> Map Pair Int -> Map Pair Int 
apply st (p1,p2) c = (insertWith (+) (p1, c) v).(insertWith (+) (c, p2) v) 
  where v = Map.findWithDefault 0 (p1,p2) st

count :: Char -> Map Pair Int -> Map Char Int
count c = (insertWith (+) c 1).Map.foldrWithKey inc Map.empty
  where inc = \(_,p2) v -> insertWith (+) p2 v
  
count' :: (Char, Char) -> Map Pair Int -> Map Char Int
count' (s,e) = Map.map (flip div 2).ends.Map.foldrWithKey inc Map.empty
  where ends = (insertWith (+) s 1).(insertWith (+) e 1)
        inc = \(p1,p2) v -> (insertWith (+) p1 v).(insertWith (+) p2 v)

main :: IO ()
main = do
  --r <- C.readFile "test.csv"
  r <- reqDay 14
  let rl = rules r
  let st = start r
  print (Map.keysSet st \\ Map.keysSet rl)
  let steps = iterate (step rl) st
  let ct = count (C.head r) $ steps !! 40
  let sted = sort.elems $ ct
  print (head sted, last sted)
  let ct' = count' (C.head r, C.last.head.C.lines $ r) $ steps !! 40
  let sted' = sort.elems $ ct'
  print (head sted', last sted')


