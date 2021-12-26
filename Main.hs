{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import Control.Monad.IO.Class
import Network.HTTP.Req

import Twenty

reqDay :: Int -> IO C.ByteString
reqDay d = fmap responseBody $ runReq defaultHttpConfig $
                   req GET (https "adventofcode.com" /: "2021" /: "day" /: (T.pack.show) d /: "input")
                       NoReqBody bsResponse $ 
                       header "Cookie" "_ga=GA1.2.476278310.1638606484; _gid=GA1.2.1700296996.1638606484; session=53616c7465645f5f5cc00fdbaca34172effc09c45d732427581595ddacb7bdd720ebc395b0eed91d5da5328f2ba5ac46" 

main :: IO ()
main = do
  r <- readFile "data/20.csv"
  run r
  r <- reqDay 20
  run (C.unpack r) 
