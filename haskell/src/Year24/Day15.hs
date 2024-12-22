module Year24.Day15 (main) where

import Data.Maybe (fromJust)
import qualified Data.Text as T
import Lib (Pos, delta, east, requestDay, toDir, west)
import qualified Matrix as M

_test :: IO T.Text
_test = T.pack <$> readFile "./src/Year24/Day15.txt"

main :: IO ()
main = do
  putStrLn "Day 15"
  input@(field, cmds) <- parse <$> requestDay 15
  -- putStrLn . M.pretty . snd $ foldl doMove (findFish field) cmds
  print $ partOne input
  -- putStrLn . M.pretty $ double $ field
  -- putStrLn . M.pretty . snd $ foldl doMove' (findFish . double $ field) cmds
  print $ partTwo input

type Field = M.Matrix Char

type State = (Pos, Field)

parse :: T.Text -> (Field, [Int])
parse input = let (field, cmds) = T.breakOn (T.pack "\n\n") . T.strip $ input in (parseField field, parseCmds cmds)
  where
    parseCmds = map toDir . T.unpack . T.concat . T.lines . T.strip
    parseField = M.fromLists . map T.unpack . T.lines

findFish :: Field -> State
findFish field = let fish = fromJust $ M.findIndex (== '@') field in (fish, M.set '.' fish field)

double :: Field -> Field
double = M.fromLists . map (concatMap char) . M.toLists
  where
    char '#' = "##"
    char 'O' = "[]"
    char '.' = ".."
    char '@' = "@."
    char _ = error "Invalid character"

boxes :: Int -> State -> Maybe Pos
boxes dir (p, a) =
  let next = p + delta dir
   in case a M.! next of
        '.' -> Just next
        '#' -> Nothing
        _ -> boxes dir (next, a)

doMove :: State -> Int -> State
doMove state@(p, a) dir =
  let next = p + delta dir
   in case a M.! next of
        '.' -> (next, a)
        '#' -> state
        _ -> case boxes dir state of
          Just end -> (next, M.set '.' next . M.set 'O' end $ a)
          Nothing -> state

score :: Field -> Int
score = M.ifoldr boxScore 0
  where
    boxScore (i, j) c = (+) (if c == 'O' then i * 100 + j else 0)

boxes' :: Int -> Field -> Pos -> Maybe [Pos]
boxes' dir a = go
  where
    go :: Pos -> Maybe [Pos]
    go p = case a M.! p of
      '.' -> Just []
      '[' -> box p east
      ']' -> box p west
      _ -> Nothing

    box :: Pos -> Int -> Maybe [Pos]
    box sideOne twoDir = do
      let sideTwo = sideOne + delta twoDir
      let nextOne = sideOne + delta dir
      let nextTwo = sideTwo + delta dir
      findOne <- go nextOne
      findTwo <- if nextTwo /= sideOne then go nextTwo else Just []
      return $ sideOne : sideTwo : (findOne ++ findTwo)

doMove' :: State -> Int -> State
doMove' state@(p, a) dir =
  let next = p + delta dir
   in case a M.! next of
        '.' -> (next, a)
        '#' -> state
        _ -> case boxes' dir a next of
          Just bs ->
            let clear = foldr (M.set '.') (M.clone a) bs
                moved = foldr (\p' -> M.set (a M.! p') (p' + delta dir)) clear bs
             in (next, moved)
          Nothing -> state

score' :: Field -> Int
score' = M.ifoldr boxScore 0
  where
    boxScore (i, j) c = (+) (if c == '[' then i * 100 + j else 0)

partOne :: (Field, [Int]) -> Int
partOne (field, cmds) = score . snd $ foldl doMove (findFish field) cmds

partTwo :: (Field, [Int]) -> Int
partTwo (field, cmds) = score' . snd $ foldl doMove' (findFish . double $ field) cmds