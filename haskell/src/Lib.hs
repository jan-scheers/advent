{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib (requestDay) where

import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Req (GET (..), NoReqBody (..), bsResponse, defaultHttpConfig, header, https, req, responseBody, runReq, (/:))

session :: B.ByteString
session = "session=53616c7465645f5f1b9c0a9bc8a5ed8359667590d2b8d1676b917ca6b30b32127bbbb350c79a8a99fb69f321070bdaa5d9ee763a8d7d2a8f91d52eacc106b054"

requestDay :: Int -> IO T.Text
requestDay day = runReq defaultHttpConfig $ do
  let url = https "adventofcode.com" /: "2024" /: "day" /: (T.pack . show) day /: "input"
  let cookie = header "Cookie" session
  r <- req GET url NoReqBody bsResponse cookie
  return . decodeUtf8 . responseBody $ r

instance (Num a) => Num (a, a) where
  (x1, y1) + (x2, y2) = (x1 + x2, y1 + y2)
  (x1, y1) * (x2, y2) = (x1 * x2, y1 * y2)
  abs (x, y) = (abs x, abs y)
  signum (x, y) = (signum x, signum y)
  fromInteger n = (fromInteger n, fromInteger n)
  negate (x, y) = (negate x, negate y)