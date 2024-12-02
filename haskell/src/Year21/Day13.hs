{-# LANGUAGE OverloadedStrings #-}

module Year21.Day13 (main) where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import Control.Monad.IO.Class
import Network.HTTP.Req
import Data.Matrix

reqDay :: Int -> IO BsResponse
reqDay d = runReq defaultHttpConfig $
                   req GET (https "adventofcode.com" /: "2021" /: "day" /: (T.pack.show) d /: "input")
                       NoReqBody bsResponse $ 
                       header "Cookie" "_ga=GA1.2.476278310.1638606484; _gid=GA1.2.1700296996.1638606484; session=53616c7465645f5f5cc00fdbaca34172effc09c45d732427581595ddacb7bdd720ebc395b0eed91d5da5328f2ba5ac46" 

type Position = (Int, Int)

parse :: C.ByteString -> Position
parse s = case map (read.C.unpack) $ C.split ',' s of [a,b] -> (b+1,a+1)

positions :: C.ByteString -> [Position]
positions = (map parse).(takeWhile (not.C.null)).C.lines

parseFold :: C.ByteString -> (Char, Int)
parseFold fs = (C.index fs 11, read.C.unpack $ C.takeWhileEnd (/= '=') fs)

folds :: C.ByteString -> [(Char, Int)]
folds = (map parseFold).tail.(dropWhile (not.C.null)).C.lines

ink :: [Position] -> Matrix Int
ink ps = foldr (unsafeSet 1) z ps 
  where z = zeros.range $ ps

range :: [Position] -> ((Int, Int), (Int, Int))
range = foldr both ((1,1),(1,1))
  where both = (\(x,y) (rx, ry) -> (cmp x rx, cmp y ry))
        cmp = (\a (mi,ma) -> (min mi a, max ma a))

zeros :: ((Int, Int), (Int, Int)) -> Matrix Int
zeros ((x1, x2), (y1, y2)) = zero (x2-x1+1) (y2-y1+1)

fold :: (Char, Int) -> Matrix Int -> Matrix Int
fold (c, i) z = forceMatrix $ elementwiseUnsafe (+) a b
  where (a, b) = case c of 'x' -> splitV i z
                           'y' -> splitH i z

splitV :: Int -> Matrix Int -> (Matrix Int, Matrix Int)
splitV v z = (left, rright)
  where rright = transpose.fromLists.reverse.toLists.transpose $ right
        left  = submatrix 1 (nrows z) 1 v z
        right = extendTo 0 0 v $ submatrix 1 (nrows z) (v+2) (ncols z) z

splitH :: Int -> Matrix Int -> (Matrix Int, Matrix Int)
splitH h z = (top, rbot)
  where rbot = fromLists.reverse.toLists $ bot
        top = submatrix 1 h 1 (ncols z) z
        bot = extendTo 0 h 0 $ submatrix (h+2) (nrows z) 1 (ncols z) z

count :: [Int] -> Int
count = foldr (\a c -> if a == 0 then c else c+1) 0

main :: IO ()
main = do
  r <- reqDay 13
  let rb = responseBody r
  let ps = positions rb
  let fs = reverse.folds $ rb
  let z = ink ps
  print fs
  let pixels = foldr fold z fs
  mapM_ print $ toLists $ fmap (\a -> if a == 0 then ' ' else '#') pixels


