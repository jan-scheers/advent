{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import Control.Monad.IO.Class
import Network.HTTP.Req
import Control.Applicative
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Data.Map (Map, (!), elems)
import qualified Data.Map as Map

reqDay :: Int -> IO BsResponse
reqDay d = runReq defaultHttpConfig $
                   req GET (https "adventofcode.com" /: "2021" /: "day" /: (T.pack.show) d /: "input")
                       NoReqBody bsResponse $ 
                       header "Cookie" "_ga=GA1.2.476278310.1638606484; _gid=GA1.2.1700296996.1638606484; session=53616c7465645f5f5cc00fdbaca34172effc09c45d732427581595ddacb7bdd720ebc395b0eed91d5da5328f2ba5ac46" 

readlines :: BsResponse -> [C.ByteString]
readlines r = C.lines $ responseBody r

parse :: [C.ByteString] -> [([String],[String])]
parse = map line 
  where line = \l -> (\[a,b] -> (part a, part b)) $ C.split '|' l
        part = \p -> filter (/= "") $ map C.unpack $ C.split ' ' p

count :: ([String],[String]) -> Int
count (_, test) = foldr ct1478 0 test
  where ct1478 = (\t acc -> if length t `elem` [2,3,4,7] then acc+1 else acc)

type Pattern = Set Char

isin :: Ord a => Set a -> Set a -> Bool
isin = Set.isSubsetOf

filter1 :: (Pattern -> Bool) -> Set Pattern -> Pattern
filter1 f pset = head.Set.toList $ Set.filter f pset

wiring :: Set Pattern ->  Map Pattern Int
wiring pset = Map.fromList $ zip [zero,one,two,three,four,five,six,seven,eight,nine] [0..] 
  where zero  = head.Set.toList.(Set.delete six).(Set.delete nine) $ sixes
        nine  = filter1 (\p -> Set.null $ four \\ p) sixes
        two   = head.Set.toList.(Set.delete five).(Set.delete three) $ fives
        five  = filter1 (\p -> Set.null $ p \\ six) fives
        three = filter1 (\p -> one `isin` p) fives
        six   = filter1 (\p -> not $ one `isin` p) sixes
        fives = Set.filter (\p -> length p == 5) pset
        sixes = Set.filter (\p -> length p == 6) pset
        eight = filter1 (\p -> length p == 7) pset 
        seven = filter1 (\p -> length p == 3) pset 
        four  = filter1 (\p -> length p == 4) pset 
        one   = filter1 (\p -> length p == 2) pset 


decimal :: [Int] -> Int
decimal bs = sum $ (*) <$> ZipList (reverse bs) <*> ZipList (map (10^) [0 ..])

decode :: ([String],[String]) -> Int
decode (fst, snd) = decimal $ map (pad !) cypher 
  where pad = wiring $ Set.fromList $ map Set.fromList fst
        cypher = map Set.fromList snd

main :: IO ()
main = do
  r <- reqDay 8
  let p = parse.readlines $ r
  print $ sum $ map decode p




