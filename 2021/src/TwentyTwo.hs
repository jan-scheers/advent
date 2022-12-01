module TwentyTwo where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Bifunctor
import Data.List (nub)
import Control.Monad
import Data.Word (Word8)

parse :: String -> [(Word8, Range, Range, Range)]
parse = map readsLine.lines

readsLine :: String -> (Word8, Range, Range, Range)
readsLine s = (if on == "on" then 1 else 0, x, y, z)
    where [on, s1]  = words s
          (x, _:s2) = head $ readsRange s1
          (y, _:s3) = head $ readsRange s2
          (z, _)    = head $ readsRange s3

readsRange :: String -> [(Range, String)]
readsRange s = do
    (_, _:s1) <- lex s
    (a, _:_:s2) <- reads s1
    (b, s') <- reads s2
    return ((a + 50, b + 50), s')

data Tensor a = Tensor { m :: Int 
                       , n :: Int 
                       , o :: Int
                       , v :: Vector a } deriving (Show)

type Ti = (Int, Int, Int)
type Range = (Int, Int)

zerosT :: Int -> Int -> Int -> Tensor Word8
zerosT i j k = Tensor i j k (V.replicate (i*j*k) 0)

toVi :: Tensor a -> Ti -> Int
toVi t (i, j, k) = i * n t * o t + j * o t + k

(!) :: Tensor a -> Ti -> a
(!) t ti = v t V.! toVi t ti

safeSlice :: Tensor a -> Range -> Range -> Range -> [Ti]
safeSlice t (i0, i1) (j0, j1) (k0, k1) = [(a, b, c) | a <- [i0..i1], a >= 0 && a < m t, 
                                                      b <- [j0..j1], b >= 0 && b < n t, 
                                                      c <- [k0..k1], c >= 0 && c < o t]

(//) :: Tensor a -> [(Ti, a)] -> Tensor a
(//) t is = t { v = V.unsafeUpd (v t) $ map (first $ toVi t) is } 

runline :: Tensor Word8 -> (Word8, Range, Range, Range) -> Tensor Word8
runline t (on, x, y, z) = t // zip (safeSlice t x y z) (repeat on)

nnz :: Vector Word8 -> Int
nnz = foldr (\a acc -> fromEnum a + acc) 0

run :: String -> IO ()
run s = do
    let p = parse s
    let t = zerosT 101 101 101
    let t' = foldl runline t p
    print.nnz $ v t'