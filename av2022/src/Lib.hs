module Lib
    ( req, file
    ) where

{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types (hCookie)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy.Char8 (unpack)

req :: Int -> IO String
req n = do
  manager <- newManager tlsManagerSettings
  url <- parseRequest $ "https://adventofcode.com/2022/day/"++show n++"/input"
  let request = url {
        requestHeaders = [(hCookie, pack "_ga=GA1.2.408844570.1669922340; _gid=GA1.2.1349233585.1669922340; ru=53616c7465645f5fe890ffa57e6e6b5f35dfe0719ee169bab1af9958f9854b9e; session=53616c7465645f5fca55e3a60f0b2daa2f4b81aeac78fbc66caafff5e41ed1cd6e8b5e09c30e0e1f44d94c917904510c78fd08bfff38be21107ac706dd739051")]
    }
  response <- httpLbs request manager
  return.unpack.responseBody $ response

file :: Int -> IO String
file n = readFile $ "src/Day"++show n++"/test.txt"