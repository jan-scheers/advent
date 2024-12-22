{-# LANGUAGE DeriveGeneric #-}

-- | Matrix datatype and operations.
--
--   Every provided example has been tested.
--   Run @cabal test@ for further tests.
module Matrix
  ( -- * Matrix type
    Matrix,
    pretty,
    prettyMatrix,
    nrows,
    ncols,
    shape,
    inRange,

    -- * Builders
    matrix,
    rowVector,
    colVector,

    -- ** Special matrices
    zero,
    identity,
    diagonalList,
    diagonal,
    permMatrix,

    -- * Conversions
    fromList,
    fromLists,
    toList,
    toLists,
    asVector,

    -- * Accessing
    get,
    (!),
    safeGet,
    safeSet,
    getRow,
    safeGetRow,
    getCol,
    safeGetCol,
    getDiag,

    -- * Manipulating matrices
    set,
    clone,
    transpose,
    setSize,
    extendTo,
    mapRow,
    mapCol,
    mapPos,
    ifoldr,
    findIndex,
  )
where

-- (<|>),
-- (<->),
-- joinBlocks,
-- flatten,

-- Classes

import Control.DeepSeq (NFData (..))
import Control.Loop (numLoop)
-- Data
import Data.Maybe (fromMaybe)
import qualified Data.Semigroup as S
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import GHC.Generics (Generic)
import Prelude hiding (foldl1)

-------------------------------------------------------
-------------------------------------------------------
---- MATRIX TYPE

encode :: Int -> (Int, Int) -> Int
{-# INLINE encode #-}
encode n (i, j) = i * n + j

decode :: Int -> Int -> (Int, Int)
{-# INLINE decode #-}
decode = flip quotRem

inRange :: Matrix a -> (Int, Int) -> Bool
inRange = inRange' . shape
  where
    inRange' (m, n) (i, j) = 0 <= i && i < m && 0 <= j && j < n

-- | Type of matrices.
--
--   Elements can be of any type. Rows and columns
--   are indexed starting by 1. This means that, if @m :: Matrix a@ and
--   @i,j :: Int@, then @m ! (i,j)@ is the element in the @i@-th row and
--   @j@-th column of @m@.
data Matrix a = M
  { -- | Number of rows.
    nrows :: {-# UNPACK #-} !Int,
    -- | Number of columns.
    ncols :: {-# UNPACK #-} !Int,
    -- | Content of the matrix as a plain vector.
    mvect :: V.Vector a
  }
  deriving (Generic)

instance (Eq a) => Eq (Matrix a) where
  m1 == m2 = mvect m1 == mvect m2

shape :: Matrix a -> (Int, Int)
shape m = (nrows m, ncols m)

-- | Just a cool way to output the shape of a matrix.
sizeStr :: Int -> Int -> String
sizeStr n m = show n ++ "x" ++ show m

-- | Display a matrix as a 'String' using the 'Show' instance of its elements.
prettyMatrix :: (Show a) => Matrix a -> String
prettyMatrix m =
  concat
    [ "┌ ",
      unwords (replicate (ncols m) blank),
      " ┐\n",
      unlines
        ["│ " ++ unwords (fmap (\j -> fill $ strings ! (i, j)) [0 .. ncols m - 1]) ++ " │" | i <- [0 .. nrows m - 1]],
      "└ ",
      unwords (replicate (ncols m) blank),
      " ┘"
    ]
  where
    strings@(M _ _ v) = fmap show m
    widest = V.maximum $ fmap length v
    fill str = replicate (widest - length str) ' ' ++ str
    blank = fill ""

pretty :: Matrix Char -> String
pretty = unlines . toLists

instance (Show a) => Show (Matrix a) where
  show = prettyMatrix

instance (NFData a) => NFData (Matrix a) where
  rnf = rnf . mvect

-------------------------------------------------------
-------------------------------------------------------
---- FUNCTOR INSTANCE

instance Functor Matrix where
  {-# INLINE fmap #-}
  fmap f (M n m v) = M n m $ V.map f v

-------------------------------------------------------
-------------------------------------------------------

-------------------------------------------------------
-------------------------------------------------------
---- MONOID INSTANCE

instance (Monoid a) => S.Semigroup (Matrix a) where
  (<>) m m' = matrix (max (nrows m) (nrows m')) (max (ncols m) (ncols m')) $ uncurry zipTogether
    where
      zipTogether row column = fromMaybe mempty $ safeGet row column m <> safeGet row column m'

instance (Monoid a) => Monoid (Matrix a) where
  mempty = fromList 1 1 [mempty]

-------------------------------------------------------
-------------------------------------------------------
-------------------------------------------------------
-------------------------------------------------------

-------------------------------------------------------
-------------------------------------------------------
---- APPLICATIVE INSTANCE
---- Works like tensor product but applies a function

instance Applicative Matrix where
  pure x = fromList 1 1 [x]
  m <*> m' = matrix (nrows m) (ncols m) $ \(i, j) -> get i j m $ get i j m'

-------------------------------------------------------
-------------------------------------------------------

-- | /O(rows*cols)/. Map a function over a row.
--   Example:
--
-- >                          ( 1 2 3 )   ( 1 2 3 )
-- >                          ( 4 5 6 )   ( 5 6 7 )
-- > mapRow (\_ x -> x + 1) 2 ( 7 8 9 ) = ( 7 8 9 )
mapRow ::
  -- | Function takes the current column as additional argument.
  (Int -> a -> a) ->
  -- | Row to map.
  Int ->
  Matrix a ->
  Matrix a
mapRow f r m =
  matrix (nrows m) (ncols m) $ \(i, j) ->
    let a = get i j m
     in if i == r
          then f j a
          else a

-- | /O(rows*cols)/. Map a function over a column.
--   Example:
--
-- >                          ( 1 2 3 )   ( 1 3 3 )
-- >                          ( 4 5 6 )   ( 4 6 6 )
-- > mapCol (\_ x -> x + 1) 2 ( 7 8 9 ) = ( 7 9 9 )
mapCol ::
  -- | Function takes the current row as additional argument.
  (Int -> a -> a) ->
  -- | Column to map.
  Int ->
  Matrix a ->
  Matrix a
mapCol f c m =
  matrix (nrows m) (ncols m) $ \(i, j) ->
    let a = get i j m
     in if j == c
          then f i a
          else a

-- | /O(rows*cols)/. Map a function over elements.
--   Example:
--
-- >                            ( 1 2 3 )   ( 0 -1 -2 )
-- >                            ( 4 5 6 )   ( 1  0 -1 )
-- > mapPos (\(r,c) a -> r - c) ( 7 8 9 ) = ( 2  1  0 )
mapPos ::
  -- | Function takes the current Position as additional argument.
  ((Int, Int) -> a -> b) ->
  Matrix a ->
  Matrix b
mapPos f m@(M {ncols = cols, mvect = vect}) =
  m {mvect = V.imap (f . decode cols) vect}

ifoldr :: ((Int, Int) -> a -> b -> b) -> b -> Matrix a -> b
ifoldr f z (M {ncols = cols, mvect = vect}) = V.ifoldr (f . decode cols) z vect

findIndex :: (a -> Bool) -> Matrix a -> Maybe (Int, Int)
findIndex f (M {ncols = n, mvect = v}) = decode n <$> V.findIndex f v

-------------------------------------------------------
-------------------------------------------------------
---- FOLDABLE AND TRAVERSABLE INSTANCES

instance Foldable Matrix where
  foldMap f = foldMap f . mvect

instance Traversable Matrix where
  sequenceA (M m n v) = M m n <$> sequenceA v

-------------------------------------------------------
-------------------------------------------------------
---- BUILDERS

-- | /O(rows*cols)/. The zero matrix of the given shape.
--
-- > zero n m =
-- >                 m
-- >   1 ( 0 0 ... 0 0 )
-- >   2 ( 0 0 ... 0 0 )
-- >     (     ...     )
-- >     ( 0 0 ... 0 0 )
-- >   n ( 0 0 ... 0 0 )
zero ::
  (Num a) =>
  -- | Rows
  Int ->
  -- | Columns
  Int ->
  Matrix a
{-# INLINE zero #-}
zero n m = M n m $ V.replicate (n * m) 0

-- | /O(rows*cols)/. Generate a matrix from a generator function.
--   Example of usage:
--
-- >                                  (  1  0 -1 -2 )
-- >                                  (  3  2  1  0 )
-- >                                  (  5  4  3  2 )
-- > matrix 4 4 $ \(i,j) -> 2*i - j = (  7  6  5  4 )
matrix ::
  -- | Rows
  Int ->
  -- | Columns
  Int ->
  -- | Generator function
  ((Int, Int) -> a) ->
  Matrix a
{-# INLINE matrix #-}
matrix n m f = M n m $ V.create $ do
  v <- MV.new $ n * m
  let en = encode m
  numLoop 0 (n - 1) $
    \i -> numLoop 0 (m - 1) $
      \j -> MV.unsafeWrite v (en (i, j)) (f (i, j))
  return v

-- | /O(rows*cols)/. Identity matrix of the given order.
--
-- > identity n =
-- >                 n
-- >   1 ( 1 0 ... 0 0 )
-- >   2 ( 0 1 ... 0 0 )
-- >     (     ...     )
-- >     ( 0 0 ... 1 0 )
-- >   n ( 0 0 ... 0 1 )
identity :: (Num a) => Int -> Matrix a
identity n = matrix n n $ \(i, j) -> if i == j then 1 else 0

-- | Similar to 'diagonalList', but using 'V.Vector', which
--   should be more efficient.
diagonal ::
  -- | Default element
  a ->
  -- | Diagonal vector
  V.Vector a ->
  Matrix a
diagonal e v = matrix n n $ \(i, j) -> if i == j then V.unsafeIndex v (i - 1) else e
  where
    n = V.length v

-- | Create a matrix from a non-empty list given the desired shape.
--   The list must have at least /rows*cols/ elements.
--   An example:
--
-- >                       ( 1 2 3 )
-- >                       ( 4 5 6 )
-- > fromList 3 3 [1..] =  ( 7 8 9 )
fromList ::
  -- | Rows
  Int ->
  -- | Columns
  Int ->
  -- | List of elements
  [a] ->
  Matrix a
{-# INLINE fromList #-}
fromList n m xs
  | n * m > V.length v =
      error $
        "List shape "
          ++ show (V.length v)
          ++ " is inconsistent with matrix shape "
          ++ sizeStr n m
          ++ " in fromList"
  | otherwise = M n m v
  where
    v = V.fromListN (n * m) xs

-- | Get the elements of a matrix stored in a list.
--
-- >        ( 1 2 3 )
-- >        ( 4 5 6 )
-- > toList ( 7 8 9 ) = [1,2,3,4,5,6,7,8,9]
toList :: Matrix a -> [a]
toList mat@(M m n _) = [get i j mat | j <- [0 .. n - 1], i <- [0 .. m - 1]]

-- | Get the elements of a matrix stored in a list of lists,
--   where each list contains the elements of a single row.
--
-- >         ( 1 2 3 )   [ [1,2,3]
-- >         ( 4 5 6 )   , [4,5,6]
-- > toLists ( 7 8 9 ) = , [7,8,9] ]
toLists :: Matrix a -> [[a]]
toLists mat@(M m n _) = [[get i j mat | j <- [0 .. n - 1]] | i <- [0 .. m - 1]]

-- | Diagonal matrix from a non-empty list given the desired shape.
--   Non-diagonal elements will be filled with the given default element.
--   The list must have at least /order/ elements.
--
-- > diagonalList n 0 [1..] =
-- >                   n
-- >   1 ( 1 0 ... 0   0 )
-- >   2 ( 0 2 ... 0   0 )
-- >     (     ...       )
-- >     ( 0 0 ... n-1 0 )
-- >   n ( 0 0 ... 0   n )
diagonalList :: Int -> a -> [a] -> Matrix a
diagonalList n e xs = matrix n n $ \(i, j) -> if i == j then xs !! (i - 1) else e

-- | Create a matrix from a non-empty list of non-empty lists.
--   /Each list must have at least as many elements as the first list/.
--   Examples:
--
-- > fromLists [ [1,2,3]      ( 1 2 3 )
-- >           , [4,5,6]      ( 4 5 6 )
-- >           , [7,8,9] ] =  ( 7 8 9 )
--
-- > fromLists [ [1,2,3  ]     ( 1 2 3 )
-- >           , [4,5,6,7]     ( 4 5 6 )
-- >           , [8,9,0  ] ] = ( 8 9 0 )
fromLists :: [[a]] -> Matrix a
{-# INLINE fromLists #-}
fromLists [] = error "fromLists: empty list."
fromLists (xs : xss) = fromList n m $ concat $ xs : fmap (take m) xss
  where
    n = 1 + length xss
    m = length xs

-- | /O(1)/. Represent a vector as a one row matrix.
rowVector :: V.Vector a -> Matrix a
rowVector v = M 1 m v
  where
    m = V.length v

-- | /O(1)/. Represent a vector as a one column matrix.
colVector :: V.Vector a -> Matrix a
colVector v = M (V.length v) 1 v

-- | /O(rows*cols)/. Permutation matrix.
--
-- > permMatrix n i j =
-- >               i     j       n
-- >   1 ( 1 0 ... 0 ... 0 ... 0 0 )
-- >   2 ( 0 1 ... 0 ... 0 ... 0 0 )
-- >     (     ...   ...   ...     )
-- >   i ( 0 0 ... 0 ... 1 ... 0 0 )
-- >     (     ...   ...   ...     )
-- >   j ( 0 0 ... 1 ... 0 ... 0 0 )
-- >     (     ...   ...   ...     )
-- >     ( 0 0 ... 0 ... 0 ... 1 0 )
-- >   n ( 0 0 ... 0 ... 0 ... 0 1 )
--
-- When @i == j@ it reduces to 'identity' @n@.
permMatrix ::
  (Num a) =>
  -- | shape of the matrix.
  Int ->
  -- | Permuted row 1.
  Int ->
  -- | Permuted row 2.
  Int ->
  -- | Permutation matrix.
  Matrix a
permMatrix n r1 r2 | r1 == r2 = identity n
permMatrix n r1 r2 = matrix n n f
  where
    f (i, j)
      | i == r1 = if j == r2 then 1 else 0
      | i == r2 = if j == r1 then 1 else 0
      | i == j = 1
      | otherwise = 0

-------------------------------------------------------
-------------------------------------------------------
---- ACCESSING

-- | /O(1)/. Get an element of a matrix. Indices range from /(1,1)/ to /(n,m)/.
--   It returns an 'error' if the requested element is outside of range.
get ::
  -- | Row
  Int ->
  -- | Column
  Int ->
  -- | Matrix
  Matrix a ->
  a
{-# INLINE get #-}
get i j (M _ n v) = v V.! encode n (i, j)

-- | Short alias for 'get'.
(!) :: Matrix a -> (Int, Int) -> a
{-# INLINE (!) #-}
m ! (i, j) = get i j m

-- | Variant of 'get' that returns Maybe instead of an error.
safeGet :: Int -> Int -> Matrix a -> Maybe a
safeGet i j a@(M n m _)
  | i > n || j > m || i < 1 || j < 1 = Nothing
  | otherwise = Just $ get i j a

-- | Variant of 'set' that returns Maybe instead of an error.
safeSet :: a -> (Int, Int) -> Matrix a -> Maybe (Matrix a)
safeSet x p@(i, j) a@(M n m _)
  | i > n || j > m || i < 1 || j < 1 = Nothing
  | otherwise = Just $ set x p a

-- | /O(1)/. Get a row of a matrix as a vector.
getRow :: Int -> Matrix a -> V.Vector a
{-# INLINE getRow #-}
getRow i (M _ m v) = V.slice (m * (i - 1)) m v

-- | Varian of 'getRow' that returns a maybe instead of an error
safeGetRow :: Int -> Matrix a -> Maybe (V.Vector a)
safeGetRow r m
  | r > nrows m || r < 1 = Nothing
  | otherwise = Just $ getRow r m

-- | /O(rows)/. Get a column of a matrix as a vector.
getCol :: Int -> Matrix a -> V.Vector a
{-# INLINE getCol #-}
getCol j (M n m v) = V.generate n $ \i -> v V.! encode m (i + 1, j)

-- | Varian of 'getColumn' that returns a maybe instead of an error
safeGetCol :: Int -> Matrix a -> Maybe (V.Vector a)
safeGetCol c m
  | c > ncols m || c < 1 = Nothing
  | otherwise = Just $ getCol c m

-- | /O(min rows cols)/. Diagonal of a /not necessarily square/ matrix.
getDiag :: Matrix a -> V.Vector a
getDiag m = V.generate k $ \i -> m ! (i + 1, i + 1)
  where
    k = min (nrows m) (ncols m)

-- | /O(rows*cols)/. Transform a 'Matrix' to a 'V.Vector' of shape /rows*cols/.
--  This is equivalent to get all the rows of the matrix using 'getRow'
--  and then append them, but far more efficient.
asVector :: Matrix a -> V.Vector a
asVector = mvect

-------------------------------------------------------
-------------------------------------------------------
---- MANIPULATING MATRICES

clone :: Matrix a -> Matrix a
clone = fromLists . toLists

-- | Replace the value of a cell in a matrix.
set ::
  -- | New value.
  a ->
  -- | Position to replace.
  (Int, Int) ->
  -- | Original matrix.
  Matrix a ->
  -- | Matrix with the given position replaced with the given value.
  Matrix a
{-# INLINE set #-}
set x (i, j) (M m n v) = M m n $ V.modify (\v' -> MV.write v' (encode n (i, j)) x) v

-- | /O(rows*cols)/. The transpose of a matrix.
--   Example:
--
-- >           ( 1 2 3 )   ( 1 4 7 )
-- >           ( 4 5 6 )   ( 2 5 8 )
-- > transpose ( 7 8 9 ) = ( 3 6 9 )
transpose :: Matrix a -> Matrix a
transpose m = matrix (ncols m) (nrows m) $ \(i, j) -> m ! (j, i)

-- | Extend a matrix to a given shape adding a default element.
--   If the matrix already has the required shape, nothing happens.
--   The matrix is /never/ reduced in shape.
--   Example:
--
-- >                            ( 1 2 3 0 0 )
-- >                ( 1 2 3 )   ( 4 5 6 0 0 )
-- >                ( 4 5 6 )   ( 7 8 9 0 0 )
-- > extendTo 0 4 5 ( 7 8 9 ) = ( 0 0 0 0 0 )
--
-- The definition of 'extendTo' is based on 'setSize':
--
-- > extendTo e n m a = setSize e (max n $ nrows a) (max m $ ncols a) a
extendTo ::
  -- | Element to add when extending.
  a ->
  -- | Minimal number of rows.
  Int ->
  -- | Minimal number of columns.
  Int ->
  Matrix a ->
  Matrix a
extendTo e n m a = setSize e (max n $ nrows a) (max m $ ncols a) a

-- | Set the shape of a matrix to given parameters. Use a default element
--   for undefined entries if the matrix has been extended.
setSize ::
  -- | Default element.
  a ->
  -- | Number of rows.
  Int ->
  -- | Number of columns.
  Int ->
  Matrix a ->
  Matrix a
{-# INLINE setSize #-}
setSize e n m a@(M n0 m0 _) = matrix n m $ \(i, j) ->
  if i <= n0 && j <= m0
    then get i j a
    else e
