module Twenty where

import Numeric.LinearAlgebra

parse :: String -> (Vector I, Matrix I)
parse s = (enh, mat)
  where input = lines s
        enh = fromList $ map c2i (head input)
        mat = fromLists $ (map.map) c2i (drop 2 input)

c2i :: Char -> I
c2i c = if c == '#' then 1 else 0

expand :: I -> Matrix I -> Matrix I
expand b m = fromBlocks [ [konst b (2,2), konst b (2,n), konst b (2,2)]
                        , [konst b (n,2), m, konst b (n,2)]
                        , [konst b (2,2), konst b (2,n), konst b (2,2)]]
  where n = rows m

pixel :: Vector I -> Matrix I -> I -> I -> I
pixel v m i j = v ! ind
  where window = subMatrix (fromIntegral i, fromIntegral j) (3,3) m
        ind = fromIntegral.binary.concat.toLists $ window

enhance :: Vector I -> (I, Matrix I) -> (I, Matrix I)
enhance v (b, m) = (b' ,build (n, n).pixel v.expand b $ m)
  where n = 2 + rows m
        b' = if b == 0 then v ! 0 else v ! 511

binary :: [I] -> I
binary = sum.zipWith (*) ((2^) <$> [0..]).reverse

run :: String -> IO ()
run r = do
  let (v, m) = parse r
  print.size $ m
  let m' = iterate (enhance v) (0,m)
  let dim = \(i,j) -> (i, size j)
  print.dim $ m' !! 2
  print.sumElements.snd $ m' !! 2
  print.dim $ m' !! 50
  print.sumElements.snd $ m' !! 50

