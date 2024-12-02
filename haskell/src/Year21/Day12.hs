{-# LANGUAGE OverloadedStrings #-}

module Year21.Day12 (main) where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import Control.Monad.IO.Class
import Network.HTTP.Req
import Data.Char (isUpper, isLower)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set, union, singleton, filter)
import qualified Data.Set as Set
import Prelude hiding (filter)

reqDay :: Int -> IO BsResponse
reqDay d = runReq defaultHttpConfig $
                   req GET (https "adventofcode.com" /: "2021" /: "day" /: (T.pack.show) d /: "input")
                       NoReqBody bsResponse $ 
                       header "Cookie" "_ga=GA1.2.476278310.1638606484; _gid=GA1.2.1700296996.1638606484; session=53616c7465645f5f5cc00fdbaca34172effc09c45d732427581595ddacb7bdd720ebc395b0eed91d5da5328f2ba5ac46" 

readlines :: BsResponse -> [C.ByteString]
readlines r = C.lines $ responseBody r

parse :: C.ByteString -> [String]
parse = map C.unpack.C.split '-'

type Cave = String
type Graph = Map Cave (Set Cave)


build :: [String] -> Graph -> Graph
build [a,b] = add a b.add b a
  where add = (\a b -> Map.insertWith union a (singleton b))

graph :: C.ByteString -> Graph
graph = foldr build Map.empty.map parse.C.lines

explore :: Graph -> Bool -> [Cave] -> Set [Cave]
explore _ _ ("end":path) = singleton ("end":path)   
explore g twice path = Set.foldr (\d acc -> union acc (explore g (twice || istwice d) $ d:path)) Set.empty next
  where istwice = \t -> not (upper t) && inpath t
        next = filter canGo $ g ! (head path)
        canGo = \t -> not (t == "start" || not (upper t) && twice && inpath t) 
        upper = \t -> all isUpper t
        inpath = \t -> t `elem` path

          
main :: IO ()
main = do
  r <- reqDay 12
  let g = graph.responseBody $ r
  print.length $ explore g False ["start"] 


