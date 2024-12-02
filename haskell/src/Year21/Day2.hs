{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Year21.Day2 (main) where

import Data.ByteString.Char8 qualified as C
import Data.Text qualified as T
import Network.HTTP.Req
  ( BsResponse,
    GET (GET),
    NoReqBody (NoReqBody),
    bsResponse,
    defaultHttpConfig,
    header,
    https,
    req,
    responseBody,
    runReq,
    (/:),
  )

type Aim = (Int, Int, Int)

type Movement = (String, Int)

moveAim :: Aim -> Movement -> Aim
moveAim (x, y, a) ("down", m) = (x, y, a + m)
moveAim (x, y, a) ("forward", m) = (x + m, y + a * m, a)
moveAim (x, y, a) (_, m) = (x, y, a - m)

reqDay :: Int -> IO BsResponse
reqDay d =
  runReq defaultHttpConfig
    $ req
      GET
      (https "adventofcode.com" /: "2021" /: "day" /: (T.pack . show) d /: "input")
      NoReqBody
      bsResponse
    $ header "Cookie" "session=53616c7465645f5f1b9c0a9bc8a5ed8359667590d2b8d1676b917ca6b30b32127bbbb350c79a8a99fb69f321070bdaa5d9ee763a8d7d2a8f91d52eacc106b054"

readlines :: BsResponse -> [String]
readlines r = map C.unpack $ C.lines $ responseBody r

readmove :: [String] -> Movement
readmove (a : b : _) = (a, read b)
readmove _ = ("", 0)

main :: IO ()
main = do
  r <- reqDay 2
  let com = map (readmove . words) $ readlines r
  let pos = foldl moveAim (0, 0, 0) com
  print com
  print pos
  print $ (\(a, b, _) -> a * b) pos
