module TwentyTwoBis where
import Data.Maybe (mapMaybe)

type Range = (Int, Int)
type Volume = (Range, Range, Range)


parse :: String -> [(Int, Volume)]
parse = map readsLine.lines

readsLine :: String -> (Int, Volume)
readsLine s = (if on == "on" then 1 else 0, c)
    where [on, s1]  = words s
          (x, _:s2) = head $ readsRange s1
          (y, _:s3) = head $ readsRange s2
          (z, _)    = head $ readsRange s3
          c = (x, y, z)

readsRange :: String -> [(Range, String)]
readsRange s = do
    (_, _:s1) <- lex s
    (a, _:_:s2) <- reads s1
    (b, s') <- reads s2
    return ((a + 50, b + 50), s')

sect :: Volume -> (Int, Volume) -> Maybe (Int, Volume)
sect (x1, y1, z1) (op, (x2, y2, z2)) = if nn x && nn y && nn z then Just (op', (x, y, z)) else Nothing
    where lo = \a b -> max (fst a) (fst b)
          hi = \a b -> min (snd a) (snd b)
          ir = \a b -> (lo a b, hi a b)
          nn = uncurry (<=)
          (x, y, z) = (ir x1 x2, ir y1 y2, ir z1 z2)
          op' = if op > 0 then -1 else 1

accCube :: (Int, Volume) -> [(Int, Volume)] -> [(Int, Volume)]
accCube (o, v) acc = if o == 0 then cs ++ acc else (o, v):cs ++ acc
    where cs = mapMaybe (sect v) acc

sumCubes :: [(Int, Volume)] -> Int
sumCubes = foldr a 0
    where a = \(o, (x, y, z)) acc -> o * s x * s y * s z + acc
          s = \(a, b) -> b - a + 1

run s = do
    let p = parse s
    let p' = foldr accCube [head p] (reverse.tail $ p)
    print $ sumCubes p'
    

