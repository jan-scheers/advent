{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}

module Year24.Day24 (main) where

import Control.Monad (guard, when)
import Data.Bits (shift, xor, (.&.), (.|.))
import Data.List (find, sort)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import Data.Vector ((!))
import qualified Data.Vector as V
import Lib (requestDay)

_test :: IO T.Text
_test = T.pack <$> readFile "./src/Year24/Day24.txt"

type Wires = Map.Map T.Text Int

type Gate = (T.Text, T.Text, T.Text, T.Text)

type Adder = ([Int], [Int])

gateOp :: Gate -> T.Text
gateOp (_, op, _, _) = op

gateOut :: Gate -> T.Text
gateOut (_, _, _, out) = out

gateIn :: Gate -> [T.Text]
gateIn (in0, _, in1, _) = [in0, in1]

parse :: T.Text -> (Wires, V.Vector Gate)
parse input =
  let (wires, gates) = T.breakOn "\n\n" input
   in (Map.fromList . map parseReg . T.lines $ wires, V.fromList $ map parseOp . T.lines . T.drop 2 $ gates)
  where
    parseReg line = let (name, val) = T.breakOn ": " line in (name, read . T.unpack . T.drop 2 $ val)
    parseOp line = let ws = T.words line in (head ws, ws !! 1, ws !! 2, ws !! 4)

main :: IO ()
main = do
  putStrLn "Day 24"
  (wires, gates) <- parse <$> requestDay 24
  mapM_ print . zip [0 :: Int ..] . V.toList $ adders gates
  mapM_ print . zip [0 :: Int ..] . V.toList $ goodSums gates
  print $ partOne wires gates

partTwo :: V.Vector Gate -> IO ()
partTwo gates = findSwap $ zip [0 ..] (getSwaps $ badGates gates)
  where
    findSwap :: [(Int, [(Int, Int)])] -> IO ()
    findSwap ((i, swap) : sw) = do
      when (i `mod` 10000 == 0) (print i)
      if checkSwap gates swap then putStrLn $ showSwap swap else findSwap sw
    findSwap [] = return ()

    showSwap =
      tail
        . init
        . show
        . sort
        . map T.unpack
        . concatMap (\(a, b) -> [gateOut (gates ! a), gateOut (gates ! b)])

checkSwap :: V.Vector Gate -> [(Int, Int)] -> Bool
checkSwap gates swaps = all isJust . goodCarries gates' $ goodSums gates'
  where
    gates' = gates V.// concatMap (doSwap gates) swaps

doSwap :: V.Vector Gate -> (Int, Int) -> [(Int, Gate)]
doSwap gates (i, j) =
  let (a, b, c, d) = gates ! i
      (e, f, g, h) = gates ! j
   in [(i, (a, b, c, h)), (j, (e, f, g, d))]

getSwaps :: [Int] -> [[(Int, Int)]]
getSwaps bad = do
  i <- [0 .. n]
  j <- [i + 1 .. n]
  k <- [j + 1 .. n]
  l <- [k + 1 .. n]
  return [swaps ! i, swaps ! j, swaps ! k, swaps ! l]
  where
    m = length bad - 1
    swaps = V.fromList $ do
      i <- [0 .. m]
      j <- [i + 1 .. m]
      return (bad !! i, bad !! j)
    n = length swaps - 1

badGates :: V.Vector Gate -> [Int]
badGates gates = filter (`notElem` goodGates) [0 .. length gates - 1]
  where
    sumGood = goodSums gates
    allGood = goodCarries gates sumGood
    goodGates = concatMap (maybe [] fst) sumGood ++ concatMap (maybe [] snd) allGood

adders :: [(Int, Int)] -> V.Vector Gate -> [Maybe Adder]
adders swaps gates = undefined
  where
    adder sw [] = (sw, [])
    adder sw (n : ns) = case if n == 0 then halfAdder gates "x00" "y00" else fullAdder gates n of
      [m@(s, _)] ->
        if gateOut (gates ! last s) == toName 'z' n
          then Just m : adder ns
          else trySwap (last s) n
      _ -> Nothing : adder ns

    trySwap g1 n = adders (swap : swaps) (gates V.// doSwap gates swap)
      where
        g2 = fromJust $ V.findIndex (\g -> gateOut g == toName 'z' n) gates
        swap = (g1, g2)

goodSums :: V.Vector Gate -> V.Vector (Maybe Adder)
goodSums gates = V.fromList $ map (onlyMatch . goodSum) [0 .. 44]
  where
    goodSum n = filter (\(s, _) -> gateOut (gates ! last s) == toName 'z' n) $ adder n
    adder n = if n == 0 then halfAdder gates "x00" "y00" else fullAdder gates n
    onlyMatch [m] = Just m
    onlyMatch _ = Nothing

goodCarries :: V.Vector Gate -> V.Vector (Maybe Adder) -> V.Vector (Maybe Adder)
goodCarries gates sums = V.fromList $ map goodCarry [0 .. 44]
  where
    goodCarry n = do
      g@(_, c) <- sums ! n
      target <-
        if n == 44
          then Just "z45"
          else do
            ([s0, s], _) <- sums ! (n + 1)
            find (\w -> w /= gateOut (gates ! s0)) $ gateIn (gates ! s)
      guard $ gateOut (gates ! last c) == target
      return g

halfAdder :: V.Vector Gate -> T.Text -> T.Text -> [Adder]
halfAdder gates x y = do
  (s, _) <- filter (matchGate [x, y] "XOR" . snd) gates'
  (c, _) <- filter (matchGate [x, y] "AND" . snd) gates'
  return ([s], [c])
  where
    gates' = V.toList $ V.indexed gates

fullAdder :: V.Vector Gate -> Int -> [Adder]
fullAdder gates n = do
  ([s0], [c0]) <- halfAdder gates x y
  (c1, _) <- filter (matchGate [gateOut $ gates ! s0] "AND" . snd) gates'
  (s, _) <- filter (matchGate [gateOut $ gates ! s0] "XOR" . snd) gates'
  (c, _) <- filter (matchGate [gateOut $ gates ! c0, gateOut $ gates ! c1] "OR" . snd) gates'
  return ([s0, s], [c0, c1, c])
  where
    gates' = V.toList $ V.indexed gates
    x = toName 'x' n
    y = toName 'y' n

toName :: Char -> Int -> T.Text
toName c n = T.pack $ c : let str = show n in replicate (2 - length str) '0' ++ str

matchGate :: [T.Text] -> T.Text -> Gate -> Bool
matchGate inWires op gate = all (\inWire -> inWire `elem` gateIn gate) inWires && op == gateOp gate

partOne :: Wires -> V.Vector Gate -> Int
partOne wires gates = sum . map num . sort . filter (T.isPrefixOf "z" . fst) . Map.toList $ sol
  where
    sol = execute (V.toList gates) wires
    num (k, v) = shift v ((read . T.unpack . T.tail $ k) :: Int)

execute :: [Gate] -> Wires -> Wires
execute [] wires = wires
execute (gate@(keyA, op, keyB, keyC) : gs) wires = case (Map.lookup keyA wires, Map.lookup keyB wires) of
  (Just valA, Just valB) ->
    let valC = case op of
          "AND" -> valA .&. valB
          "OR" -> valA .|. valB
          _ -> valA `xor` valB
     in execute gs (Map.insert keyC valC wires)
  _ -> execute (gs ++ [gate]) wires
