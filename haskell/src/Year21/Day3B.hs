{-# LANGUAGE OverloadedStrings #-}

module Year21.Day3B (main) where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import Control.Monad.IO.Class
import Control.Applicative
import Network.HTTP.Req
import Distribution.Simple.Utils (xargs)
import Data.Bool (Bool)
import Distribution.SPDX (LicenseId(AFL_3_0))

type Position = (Int, Int)
type Aim = (Int, Int, Int)
type Movement = (String, Int)

reqDay :: Int -> IO BsResponse
reqDay d = runReq defaultHttpConfig $
                   req GET (https "adventofcode.com" /: "2021" /: "day" /: (T.pack.show) d /: "input")
                       NoReqBody bsResponse $ 
                       header "Cookie" "_ga=GA1.2.476278310.1638606484; _gid=GA1.2.1700296996.1638606484; session=53616c7465645f5f5cc00fdbaca34172effc09c45d732427581595ddacb7bdd720ebc395b0eed91d5da5328f2ba5ac46" 


readlines :: BsResponse -> [C.ByteString]
readlines r = C.lines $ responseBody r

intify :: C.ByteString -> [Int]
intify b = map (\x -> if x == '1' then 1 else 0) $ C.unpack b

binary :: [Int] -> Int
binary bs = sum $ (*) <$> ZipList (reverse bs) <*> ZipList (map (2^) [0 ..])

most :: [[Int]] -> Int -> [Int]
most [a] _ = a
most a i   = most f (i+1)
  where f = filter (\a -> a !! i == b) a
        b = if mostly1 a i then 1 else 0

least :: [[Int]] -> Int -> [Int]
least [a] _ = a
least a i   = least f (i+1)
  where f = filter (\a -> a !! i == b) a
        b = if mostly1 a i then 0 else 1

sumBool :: [Bool] -> Int
sumBool = foldr (\b a-> if b then a+1 else a) 0

mostly1 :: [[Int]] -> Int -> Bool
mostly1 a i = 2 * sumBool c >= length a
  where c = map (\a -> a !! i == 1) a

main :: IO ()
main = do
  r <- reqDay 3
  let a = map intify $ readlines r
  print $ most a 0
  print $ binary $ most a 0
  print $ least a 0
  print $ binary $ least a 0
  print $ binary (most a 0) * binary (least a 0)






