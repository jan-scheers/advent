module Day6.Run (run) where

import Lib (file)
import Data.List (nub)

look :: String -> Int
look = uncurry (lookNext 14).splitAt 14

lookNext :: Int -> String -> String -> Int
lookNext i hd@(_:hs) (t:tl) = if nub hd == hd then i else lookNext (i + 1) (hs ++ [t]) tl 
lookNext _ _ _ = -1

run :: IO ()
run = do
    r <- file 6
    mapM_ (putStrLn.show.look) $ lines r