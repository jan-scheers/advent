{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Year21.Day1 (main) where

import Control.Applicative
import Data.ByteString.Char8 qualified as C
import Network.HTTP.Req

parse :: BsResponse -> [Int]
parse r = map (read . C.unpack) $ C.lines $ responseBody r

cmpnext :: (Ord a) => [a] -> [Bool]
cmpnext a = getZipList $ (>) <$> ZipList (drop 1 a) <*> ZipList a

window3 :: (Num a) => [a] -> [a]
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
      $ header "Cookie" "session=53616c7465645f5f1b9c0a9bc8a5ed8359667590d2b8d1676b917ca6b30b32127bbbb350c79a8a99fb69f321070bdaa5d9ee763a8d7d2a8f91d52eacc106b054"
  print $ sum $ map fromEnum $ cmpnext . parse $ r
  print $ sum $ map fromEnum $ cmpnext . window3 . parse $ r
  print $ length $ parse r
