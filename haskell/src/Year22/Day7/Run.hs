{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Year22.Day7.Run (run) where
import Year22.Lib (req)
import Control.Monad (liftM)
import Data.Maybe ( catMaybes )


type Path = [String]
data Tree = File {name :: String, val :: Int} | Dir {name :: String, contents :: [Tree]}
instance Show Tree where
    show (File name size) = "- " ++ name ++ " (file, size="++show size++")\n"
    show (Dir name content) = "- " ++ name ++ " (dir)\n"++concatMap (unlines.map ((++) "  ").lines.show) content

root :: Tree
root = Dir "/" []

build :: [String] -> ([String], Tree) -> Tree
build (line:tape) (path, tree) = let cmd = words line in if cmd !! 1 == "cd"
    then build tape $ (case cmd !! 2 of
        ".." -> (tail path, tree)
        cd -> (cd:path, tree))
    else let (dir, rest) = span ((/='$').head) tape
        in build rest (path, mv (Dir (head path) (ls dir)) (reverse path) tree)
build [] (_, tr) = tr

mv :: Tree -> Path -> Tree -> Tree
mv node [cd] curr = if cd /= name curr then curr else node
mv node (cd:path) curr = if cd /= name curr then curr else case curr of
    Dir nm nodes -> Dir nm $ map (mv node path) nodes
    nd -> nd
mv node [] _ = node

ls :: [String] -> [Tree]
ls = map (leaf.words)

leaf :: [String] -> Tree
leaf [size, name] = if size == "dir" then Dir name [] else File name (read size)
leaf _ = root


du :: Tree -> Int
du (Dir _ childs) = sum $ map du childs
du (File _ size) = size 

du2 :: Tree -> Int
du2 pr@(Dir _ childs) =  (let tot = du pr in if tot <= 100000 then tot else 0) + (sum $ map du2 childs)
du2 _ = 0

free :: Int -> Tree -> [Maybe Int]
free target pr@(Dir _ childs) = let tot = du pr in
    if (tot < target) then [Nothing] else (Just tot):(concatMap (free target) childs)
free _ _ = [Nothing]

run :: IO ()
run = do
    script <- liftM lines.req $ 7
    let dir = build script ([], root)
    putStrLn.show $ dir
    let target = du dir - 40000000
    putStrLn.show.minimum.catMaybes $ free target dir

