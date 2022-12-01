module TwentyFour where
import Data.List(elemIndex)
import Data.List.Split (chunksOf)
import Data.Char (isLetter, isNumber)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

data Var = Reg { reg :: Char } | Lit { val :: Int } deriving (Eq)

instance Show Var where
    show (Reg c) = [c]
    show (Lit l) = show l

type Register = Map Char Int

type Vars = (Int, Int, Int)

type Stack = ([Int], [Int])

data Op = Op { func :: String
             , var1 :: Var
             , var2 :: Var } deriving (Eq) 
instance Show Op where
    show o = func o ++ ' ':show (var1 o) ++ ' ':show (var2 o) ++ " "

parse :: String -> [[Op]]
parse = (map.map) getOp.chunksOf 18.lines 

eql :: Eq a => a -> a -> Int
eql a b = fromEnum (a == b)

getOp :: String -> Op
getOp ('i':'n':'p':_) = Op "inp" (Reg 'w') (Reg 'i')
getOp s = Op op v1 v2
    where op = take 3 s
          v1 = Reg (s !! 4)
          v2 = if isLetter (s !! 6) 
                then Reg (s !! 6) 
                else Lit (read $ drop 6 s)

chunk :: Register -> [Op] -> Register
chunk r = foldr step r.reverse

clear :: Int -> Register
clear i = M.insert 'i' i.M.fromList $ zip "xyzw" (repeat 0)

step :: Op -> Register -> Register
step op r = M.insert (reg.var1 $ op) (runOp op r) r

runOp :: Op -> Register -> Int
runOp (Op op r1 r2) r 
    | op == "inp" = v2
    | op == "add" = v1 + v2 
    | op == "mul" = v1 * v2
    | op == "div" = div v1 v2
    | op == "mod" = mod v1 v2
    | op == "eql" = eql v1 v2
    | otherwise = error "Unknown op"
    where v1 = get r r1
          v2 = get r r2

get :: Register -> Var -> Int
get _ (Lit l) = l
get r (Reg c) = r ! c

vars :: [Op] -> Vars
vars op = (a, b, c)
    where a = val.var2 $ (op !! 4)
          b = val.var2 $ (op !! 5)
          c = val.var2 $ (op !! 15)

analytic:: Vars -> Int -> Int -> Int
analytic (a, b, c) i z = div z a * (25 * x + 1) + x * (i + c)
    where x = fromEnum $ mod z 26 + b /= i

runStack :: Vars -> [Stack] -> [Stack]
runStack (a, b, c) = if a == 1 
    then concatMap (push c) 
    else pop b

push :: Int -> Stack -> [Stack]
push c (is, st) = map zipStack [1..9]
    where zipStack = \i -> (i:is, c + i:st)

pop :: Int -> [Stack] -> [Stack]
pop b = filter canPop.map doPop 
    where canPop = \(i:_, _) -> i >= 1 && i <= 9
          doPop = \(is, s:st) -> (s + b:is, st)
          

range :: [Op] -> Int -> [Int]
range op z = map (\i -> analytic (vars op) i z) [1..9]

findPair :: Int -> [Op] -> Int -> Maybe (Int, Int)
findPair z' op z = case elemIndex z' $ range op z of
    Nothing -> Nothing
    Just i -> Just (i+1, z)

makeZ :: Int -> [Op] -> [(Int, Int)]
makeZ z' op = take 9 $ mapMaybe (findPair z' op)  [0..1000]

run :: IO ()
run = do
    r <- readFile "data/24.csv"
    let v = map vars $ parse r
    let id = foldr runStack [([0], [0])] (reverse v)            
    print.filter isNumber.show.tail.reverse.fst.head $ id
    print.filter isNumber.show.tail.reverse.fst.last $ id
