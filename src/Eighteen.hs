{-# LANGUAGE LambdaCase #-}
module Eighteen where
import Data.Maybe
import qualified Data.ByteString.Char8 as C

data Snail v = Val v | Pair (Snail v) (Snail v)

instance (Show v) => Show (Snail v) where
  show (Val v) = show v
  show (Pair s1 s2) = "["++show s1++","++show s2++"]"

instance (Read v) => Read (Snail v) where
  readsPrec _ v = readsSnail v

readsSnail :: (Read a) => ReadS (Snail a)
readsSnail s = do
  ("[",s1) <- lex s
  (l,  s2) <- readsSnail s1
  (",",s3) <- lex s2
  (r,  s4) <- readsSnail s3
  ("]",s') <- lex s4
  return (Pair l r, s')
  ++ do
  (v, s') <- reads s
  return (Val v, s')

data Crumb s = WentLeft (Snail s) | WentRight (Snail s) deriving (Show)
type Zip s = (Snail s, [Crumb s])

isVal :: Snail a -> Bool
isVal (Val v) = True
isVal _ = False

isLeft :: Crumb s -> Bool
isLeft (WentLeft s) = True
isLeft (WentRight s) = False

isRight :: Crumb s -> Bool
isRight = not.isLeft

goLeft :: Zip s -> Zip s
goLeft (Pair s1 s2, cs) = (s1, WentLeft s2:cs)
goLeft (Val v, cs) = (Val v, cs)

goRight :: Zip s -> Zip s
goRight (Pair s1 s2, cs) = (s2, WentRight s1:cs)
goRight (Val v, cs) = (Val v, cs)

leaf :: (Zip s -> Zip s) -> Zip s -> Zip s
leaf f = head.dropWhile (\(s,_) -> not.isVal $ s).iterate f

goUp :: Zip s -> Zip s
goUp (s, (WentLeft r):cs) = (Pair s r, cs)
goUp (s, (WentRight l):cs) = (Pair l s, cs)
goUp (s, []) = (s, [])

zipUp :: Zip s -> Snail s
zipUp = fst.head.dropWhile (\(_,a) -> not.null $ a).iterate goUp

prev :: Zip Int -> Maybe (Zip Int)
prev (s, cs) = if null cls
  then Nothing
  else Just (leaf goRight.goLeft.goUp $ (crt, cls))
  where (crt, cls) = head.dropWhile (\case (_,[]) -> False
                                           (_,c:cs) -> isLeft c ).iterate goUp $ (s, cs)

next :: Zip Int -> Maybe (Zip Int)
next (s, cs) = if null cls
  then Nothing
  else Just (leaf goLeft.goRight.goUp $ (crt, cls))
  where (crt, cls) = head.dropWhile (\case (_,[]) -> False
                                           (_,c:cs) -> isRight c ).iterate goUp $ (s, cs)

might :: Maybe a -> Maybe a -> Maybe a
might a b = if isJust a then a else b

explode :: Zip Int -> Maybe (Zip Int)
explode z = if length depth < 4 || isRight went
  then next z >>= explode
  else new''
  where (_, went:depth) = z
        (Pair (Val l) (Val r), trail) = goUp z
        new = Just (Val 0, trail)
        new' = might (new >>= prev >>= \(Val s, tr) -> return (Val (s+l), tr) >>= next) new
        new''= might (new'>>= next >>= \(Val s, tr) -> return (Val (s+r), tr) >>= prev) new'

split :: Zip Int -> Maybe (Zip Int)
split z = if v < 10
  then next z >>= split
  else Just new
  where (Val v, tr) = z
        (q, r) = quotRem v 2
        new = (Pair (Val q) (Val (q+r)), tr)

action :: Snail Int -> Maybe (Snail Int)
action s = zipUp <$> might (explode start) (split start)
  where start = leaf goLeft (s,[])

count :: Snail Int -> Int
count (Val v) = v
count (Pair l r) = 3 * count l + 2 * count r

run :: C.ByteString -> Int
run = count.foldr1 runLine.snails
  where runAction = maybeLast.takeWhile isJust.iterate (>>= action)
        maybeLast = \ls -> if null ls then Nothing else last ls
        runLine = \r l -> let js = Just (Pair l r) in fromJust $ might (runAction js) js
        snails = map (read.C.unpack).reverse.C.lines

run' :: C.ByteString -> Int
run' bs = maximum $ [count (runLine l r) | l <- snails, r <- snails]
  where runAction = maybeLast.takeWhile isJust.iterate (>>= action)
        maybeLast = \ls -> if null ls then Nothing else last ls
        runLine = \l r -> let js = Just (Pair l r) in fromJust $ might (runAction js) js
        snails = map (read.C.unpack).reverse.C.lines $ bs
