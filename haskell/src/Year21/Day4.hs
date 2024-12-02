{-# LANGUAGE OverloadedStrings #-}

module Year21.Day4 (main) where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import Control.Applicative
import Data.List.Split
import Data.Matrix
import Control.Monad.IO.Class
import Network.HTTP.Req

reqDay :: Int -> IO BsResponse
reqDay d = runReq defaultHttpConfig $
                   req GET (https "adventofcode.com" /: "2021" /: "day" /: (T.pack.show) d /: "input")
                       NoReqBody bsResponse $ 
                       header "Cookie" "_ga=GA1.2.476278310.1638606484; _gid=GA1.2.1700296996.1638606484; session=53616c7465645f5f5cc00fdbaca34172effc09c45d732427581595ddacb7bdd720ebc395b0eed91d5da5328f2ba5ac46" 

readlines :: BsResponse -> [C.ByteString]
readlines r = C.lines $ responseBody r

readArray :: C.ByteString -> [Int]
readArray b = map (read.C.unpack) $ C.split ',' b

readRow :: C.ByteString -> [Int]
readRow b = map (read.C.unpack) $ C.words b

parselines :: [C.ByteString] -> ([Int],[Matrix Int])
parselines input = (balls, cards)
  where balls = readArray $ (head.head) sp
        cards = map (fromLists.map readRow) $ tail sp
        sp = split (dropDelims $ whenElt (== C.empty)) input

check :: Matrix Int -> [Int] -> Bool
check card balls = isBingo $ hits card balls

hits :: Matrix Int -> [Int] -> Matrix Bool
hits card balls = fmap (`elem` balls) card

isBingo :: Matrix Bool -> Bool
isBingo mat = checkRows || checkCols
  where checkRows     = any (\i -> and $ getRow i mat) [1..n]
        checkCols     = any (\i -> and $ getCol i mat) [1..n]
        --checkDiag     = foldr1 (&&) $ getDiag mat
        --checkAntiDiag = foldr1 (&&) $ map (\i -> unsafeGet i (n+1-i) mat) [1..n]
        n = 5

getBingo :: [Int] -> [Matrix Int] -> ([Int], Matrix Int)
getBingo balls cards = head $ dropWhile (\(b,c) -> not $ check c b) combine
  where combine = liftA2 (,) ballsseq cards
        ballsseq = map (`take` balls) [1..length balls]

getLastBingo :: [Int] -> [Matrix Int] -> ([Int], Matrix Int)
getLastBingo balls cards = foldr1 cmp bingos
  where bingos = map (\c -> getBingo balls [c]) cards
        cmp = \a b -> if len a >= len b then a else b
        len = \(a,_) -> length a

sumBingo :: Matrix Int -> [Int] -> Int
sumBingo card ball = sum $ zipWith lam (toList $ hits card ball) (toList card)
  where lam = \a b -> if not a then b else 0

main :: IO ()
main = do
  r <- reqDay 4
  -- r <- readFile "test.csv"
  let (balls,cards) = parselines.readlines $ r
  let (ball,card) = getBingo balls cards
  print (ball,card)
  print $ sumBingo card ball
  print $ last ball
  print $ sumBingo card ball * last ball




