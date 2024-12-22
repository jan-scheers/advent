{-# LANGUAGE OverloadedStrings #-}

module Year24.Day17 (main) where

import Control.Monad (when)
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
  (tape, regs) <- parse <$> _test
  run tape regs 0

type Tape = V.Vector (Int, Int)

type Register = V.Vector Int

parse :: T.Text -> (Tape, Register)
parse input = (parseTape tape, parseRegs regs)
  where
    (regs, tape) = T.breakOn "\n\n" . T.strip $ input
    parseRegs = V.fromList . map parseReg . T.lines
    parseReg = read . T.unpack . T.drop (T.length "Register X: ")

    parseTape = V.fromList . perTwo . parseOps
    parseOps = map (read . T.unpack) . T.splitOn "," . T.drop (T.length "\n\nProgram: ")

    perTwo [] = []
    perTwo [_] = []
    perTwo (x : y : xs) = (x, y) : perTwo xs

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

run :: Tape -> Register -> Int -> IO ()
run tape reg ip =
  if ip >= V.length tape
    then putStrLn ""
    else do
      let (op, arg) = tape ! ip
      let jnz = (op == 3) && ((reg ! 0) /= 0)
      when (op == 5) $ putStr $ show (combo reg arg `mod` 8) ++ ","
      run tape (ex op arg reg) (if jnz then arg else ip + 1)
