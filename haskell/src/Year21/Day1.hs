{-# LANGUAGE OverloadedStrings #-}

module Year21.Day1 (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C
import Network.HTTP.Req

parse :: BsResponse -> [Int]
parse r = map (read . C.unpack) $ C.lines $ responseBody r

cmpnext :: Ord a => [a] -> [Bool]
cmpnext a = getZipList $ (>) <$> ZipList (drop 1 a) <*> ZipList a

window3 :: Num a => [a] -> [a]
window3 a = getZipList $ liftA3 (\a b c -> a + b + c) (ZipList (drop 2 a)) (ZipList (drop 1 a)) (ZipList a)

main :: IO ()
main = do
  r <-
    runReq defaultHttpConfig
      $ req
        GET
        (https "adventofcode.com" /: "2021" /: "day" /: "1" /: "input")
        NoReqBody
        bsResponse
      $ header "Cookie" "_ga=GA1.2.476278310.1638606484; _gid=GA1.2.1700296996.1638606484; session=53616c7465645f5f5cc00fdbaca34172effc09c45d732427581595ddacb7bdd720ebc395b0eed91d5da5328f2ba5ac46"
  print $ sum $ map fromEnum $ cmpnext . parse $ r
  print $ sum $ map fromEnum $ cmpnext . window3 . parse $ r
  print $ length $ parse r
