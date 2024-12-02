{-# LANGUAGE OverloadedStrings #-}

module Year21.Day10 (main) where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import Control.Monad.IO.Class
import Network.HTTP.Req
import Data.Maybe (catMaybes)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.List (sort)


reqDay :: Int -> IO BsResponse
reqDay d = runReq defaultHttpConfig $
                   req GET (https "adventofcode.com" /: "2021" /: "day" /: (T.pack.show) d /: "input")
                       NoReqBody bsResponse $ 
                       header "Cookie" "_ga=GA1.2.476278310.1638606484; _gid=GA1.2.1700296996.1638606484; session=53616c7465645f5f5cc00fdbaca34172effc09c45d732427581595ddacb7bdd720ebc395b0eed91d5da5328f2ba5ac46" 

readlines :: BsResponse -> [C.ByteString]
readlines r = C.lines $ responseBody r

readlines' :: String -> [C.ByteString]
readlines' = C.lines.C.pack 

open :: [Char]
open =  ['(','[','{','<']

close :: [Char]
close = [')',']','}','>'] 

match :: Map Char Char
match = Map.fromList $ zip open close 

s1map :: Map Char Int
s1map = Map.fromList $ zip close [3,57,1197,25137]

s2map :: Map Char Int
s2map = Map.fromList $ zip open [1..4]

stepStack :: Char -> (Maybe Char, [Char]) -> (Maybe Char, [Char])
stepStack c (t, [])        = (t, [c])
stepStack c (Just a, s:st) = (Just a, s:st)
stepStack c (Nothing, s:st) | c `elem` open     = (Nothing, c:s:st)
                        | c `elem` close && c == match ! s = (Nothing, st)
                        | otherwise         = (Just c, st)

illegal :: [Char] -> Maybe Char
illegal s  = case foldr stepStack (Nothing, []) (reverse s) of (c,_) -> c

complete :: [Char] -> Maybe [Char]
complete s = case foldr stepStack (Nothing, []) (reverse s) of 
             (Nothing, st) -> Just st
             (Just c,_) -> Nothing

score1 :: [Char] -> Int
score1 s = sum $ map (s1map !) s

score2 :: [Char] -> Int
score2 s = foldl (\acc c -> 5*acc + s2map ! c) 0 s

main :: IO ()
main = do
  --r <- readFile "ten.csv"
  r <- reqDay 10
  let a = catMaybes.map (complete.C.unpack) $ readlines r
  let scores = map score2 a
  print $ sort scores !! div (length scores) 2
