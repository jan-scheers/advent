{-# LANGUAGE OverloadedStrings #-}

module Year24.Day19 (main) where

import qualified Data.Text as T
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import Lib (requestDay)

_test :: IO T.Text
_test = T.pack <$> readFile "./src/Year24/Day19.txt"

main :: IO ()
main = do
  putStrLn "Day 19"
  (patterns, designs) <- parse <$> requestDay 19
  putStrLn . T.unpack . T.unwords $ patterns
  mapM_
    ( \d -> do
        putStrLn $ T.unpack d ++ " " ++ show (match patterns d)
    )
    designs
  partOne patterns designs
  partTwo patterns designs

partOne :: [T.Text] -> [T.Text] -> IO ()
partOne ps = print . length . filter ((> 0) . match ps)

partTwo :: [T.Text] -> [T.Text] -> IO ()
partTwo ps = print . sum . map (match ps)

parse :: T.Text -> ([T.Text], [T.Text])
parse = parse' . break T.null . T.lines
  where
    parse' (a, b) = (parsePatterns a, parseDesigns b)
    parsePatterns = T.splitOn ", " . head
    parseDesigns = filter (not . T.null)

match :: [T.Text] -> T.Text -> Int
match patterns design = V.last solve
  where
    start = V.replicate (T.length design + 1) 0 // [(0, 1)]
    solve = foldl match' start [0 .. T.length design]

    match' :: Vector Int -> Int -> Vector Int
    match' dp i =
      dp
        // ( map
               (\p -> (i + T.length p, dp ! i + dp ! (i + T.length p)))
               . filter (`T.isPrefixOf` T.drop i design)
               $ patterns
           )