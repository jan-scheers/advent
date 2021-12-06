{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import Control.Monad.IO.Class
import Network.HTTP.Req

type Position = (Int, Int)
type Aim = (Int, Int, Int)
type Movement = (String, Int)

move :: Position -> Movement -> Position
move (x,y) ("up", m)      = (x,y-m)
move (x,y) ("down", m)    = (x,y+m)
move (x,y) ("forward", m) = (x+m,y)

moveAim :: Aim -> Movement -> Aim
moveAim (x,y,a) ("up", m)      = (x,y,a-m)
moveAim (x,y,a) ("down", m)    = (x,y,a+m)
moveAim (x,y,a) ("forward", m) = (x+m,y+a*m,a)

req_day :: Int -> IO BsResponse
req_day d = runReq defaultHttpConfig $
                   req GET (https "adventofcode.com" /: "2021" /: "day" /: (T.pack.show) d /: "input")
                       NoReqBody bsResponse $ 
                       header "Cookie" "_ga=GA1.2.476278310.1638606484; _gid=GA1.2.1700296996.1638606484; session=53616c7465645f5f5cc00fdbaca34172effc09c45d732427581595ddacb7bdd720ebc395b0eed91d5da5328f2ba5ac46" 


readlines :: BsResponse -> [String]
readlines r = map C.unpack $ C.lines $ responseBody r

readmove :: [String] -> Movement
readmove (a:b:_) = (a, read b)

main :: IO ()
main = do
  r <- req_day 2
  com <- return $ map (readmove.words) $ readlines r
  pos <- return $ foldl moveAim (0,0,0) com
  print com
  print pos
  print $ (\ (a,b,_) -> a*b) pos




