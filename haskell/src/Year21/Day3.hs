{-# LANGUAGE OverloadedStrings #-}

module Year21.Day3 (main) where

import Control.Applicative
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import Network.HTTP.Req

reqDay :: Int -> IO BsResponse
reqDay d =
  runReq defaultHttpConfig
    $ req
      GET
      (https "adventofcode.com" /: "2021" /: "day" /: (T.pack . show) d /: "input")
      NoReqBody
      bsResponse
    $ header "Cookie" "session=53616c7465645f5f1b9c0a9bc8a5ed8359667590d2b8d1676b917ca6b30b32127bbbb350c79a8a99fb69f321070bdaa5d9ee763a8d7d2a8f91d52eacc106b054"

readlines :: BsResponse -> [C.ByteString]
readlines r = C.lines $ responseBody r

intify :: C.ByteString -> [Int]
intify b = map (\x -> if x == '1' then 1 else 0) $ C.unpack b

add :: [Int] -> [Int] -> [Int]
add a b = getZipList $ (+) <$> ZipList a <*> ZipList b

binary :: [Int] -> Int
binary bs = sum $ (*) <$> ZipList (reverse bs) <*> ZipList (map (2 ^) [0 ..])

main :: IO ()
main = do
  r <- reqDay 3
  let c = foldr1 add (map intify $ readlines r)
  let b = map (round . (/ (fromIntegral . length) (readlines r)) . fromIntegral) c
  print b
  print $ binary b
  print $ binary (map (1 -) b)
  print $ binary b * binary (map (1 -) b)
