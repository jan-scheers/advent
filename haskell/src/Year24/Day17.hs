{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Year24.Day17 (main) where

import Control.Monad (guard, when)
import Data.Bits (Bits (shift, xor))
import qualified Data.Text as T
import Data.Vector ((!), (//))
import qualified Data.Vector as V
import Lib (requestDay)

_test :: IO T.Text
_test = T.pack <$> readFile "./src/Year24/Day17.txt"

main :: IO ()
main = do
  putStrLn "Day 17"
  (tape, regs) <- parse <$> requestDay 17
  mapM_ (putStrLn . decompile) . V.toList . parseTape $ tape
  simulate (parseTape tape) regs 0
  print . run . toStack $ regs ! 0
  print tape
  let a = solve tape
  print . minimum $ map toNumber a

type Tape = V.Vector (Int, Int)

type Register = V.Vector Int

parseTape :: [Int] -> Tape
parseTape = V.fromList . perTwo
  where
    perTwo [] = []
    perTwo [_] = []
    perTwo (x : y : xs) = (x, y) : perTwo xs

parse :: T.Text -> ([Int], Register)
parse input = (parseOps tape, parseRegs regs)
  where
    (regs, tape) = T.breakOn "\n\n" . T.strip $ input
    parseRegs = V.fromList . map parseReg . T.lines
    parseReg = read . T.unpack . T.drop (T.length "Register X: ")

    parseOps = map (read . T.unpack) . T.splitOn "," . T.drop (T.length "\n\nProgram: ")

combo :: Register -> Int -> Int
combo reg op = if op < 4 then op else reg V.! (op - 4)

adv :: Int -> Register -> Register
adv arg reg = reg // [(0, (reg ! 0) `div` shift 1 (combo reg arg))]

bdv :: Int -> Register -> Register
bdv arg reg = reg // [(1, (reg ! 0) `div` shift 1 (combo reg arg))]

cdv :: Int -> Register -> Register
cdv arg reg = reg // [(2, (reg ! 0) `div` shift 1 (combo reg arg))]

bxl :: Int -> Register -> Register
bxl arg reg = reg // [(1, (reg ! 1) `xor` arg)]

bst :: Int -> Register -> Register
bst arg reg = reg // [(1, combo reg arg `mod` 8)]

bxc :: Int -> Register -> Register
bxc _ reg = reg // [(1, (reg ! 1) `xor` (reg ! 2))]

idt :: Int -> Register -> Register
idt _ reg = reg

ex :: Int -> Int -> Register -> Register
ex op = case op of
  0 -> adv
  1 -> bxl
  2 -> bst
  4 -> bxc
  6 -> bdv
  7 -> cdv
  _ -> idt

simulate :: Tape -> Register -> Int -> IO ()
simulate tape reg ip =
  if ip >= V.length tape
    then putStrLn ""
    else do
      let (op, arg) = tape ! ip
      let jnz = (op == 3) && ((reg ! 0) /= 0)
      when (op == 5) $ putStr $ show (combo reg arg `mod` 8) ++ ","
      simulate tape (ex op arg reg) (if jnz then arg else ip + 1)

combo' :: Int -> String
combo' arg = case arg of
  4 -> "A"
  5 -> "B"
  6 -> "C"
  _ -> show arg

decompile :: (Int, Int) -> String
decompile (op, arg) = case op of
  0 -> "A / 2^" ++ combo' arg ++ " -> A"
  1 -> "B xor " ++ show arg ++ " -> B"
  2 -> combo' arg ++ " mod 8 -> B"
  3 -> "if A goto " ++ show arg
  4 -> "B xor C -> B"
  5 -> "print " ++ combo' arg ++ " mod 8"
  6 -> "A / 2^" ++ combo' arg ++ " -> B"
  7 -> "A / 2^" ++ combo' arg ++ " -> C"
  _ -> "noop"

type Stack = [Int]

toStack :: Int -> Stack
toStack 0 = []
toStack n = n `mod` 8 : toStack (n `div` 8)

toNumber :: Stack -> Int
toNumber = sum . zipWith (\i a -> shift a (3 * i)) [0 ..]

solve :: Stack -> [Stack]
solve tape = solve' (reverse tape) []
  where
    solve' :: Stack -> Stack -> [Stack]
    solve' [] as = return as
    solve' (z : zs) as = do
      a <- [0 .. 7]
      guard $ eval (a : as) == z
      solve' zs (a : as)

eval :: Stack -> Int
eval [] = 0
eval st@(b : _) =
  let (a, b') = (toNumber st, b `xor` 1)
   in (shift a (-b') `xor` (b' `xor` 5)) `mod` 8

run :: Stack -> Stack
run [] = []
run st@(_ : rest) = eval st : run rest