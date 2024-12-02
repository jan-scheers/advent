{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import Control.Monad.IO.Class
import Control.Applicative
import Network.HTTP.Req

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

add :: [Int] -> [Int] -> [Int]
add a b = getZipList $ (+) <$> ZipList a <*> ZipList b

binary :: [Int] -> Int
binary bs = sum $ (*) <$> ZipList (reverse bs) <*> ZipList (map (2^) [0 ..])

main :: IO ()
main = do
  r <- reqDay 3
  let c = foldr1 add (map intify $ readlines r)
  let b = map (round.(/ (fromIntegral.length) (readlines r)).fromIntegral) c
  print b
  print $ binary b
  print $ binary (map (1-) b)
  print $ binary b * binary (map (1-) b)





