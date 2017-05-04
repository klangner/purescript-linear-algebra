module LinearAlgebra.Matrix 
  ( Matrix 
  -- * Constructors
  , fromArray
  , replicate
  , zeros
  -- * Access data
  , ncols
  , nrows
  , column
  , element
  , row
  , columns
  , rows
  -- * Mapping over matrix
  ) where

import Prelude
import Data.Array as A 
import Data.Maybe (Maybe(..))
import LinearAlgebra.Vector (Vector)


-- | Dense Matrix implementation
data Matrix a = Dense Int Int (Array a)  -- nrow, ncol, values

-- | Number of rows in matrix
nrows :: ∀ a. Matrix a -> Int 
nrows (Dense r _ _) = r

-- | Number of cols in matrix
ncols :: ∀ a. Matrix a -> Int 
ncols (Dense _ c _) = c


-- | Create array of given dimmension containing replicated value
replicate :: ∀ a. Int -> Int -> a -> Maybe (Matrix a )
replicate r c v | r > 0 && c > 0 = Just $ Dense r c (A.replicate (r * c) v)
                | otherwise = Nothing


-- | Create array of given dimmension with all values set to 0
-- | 
zeros :: Int -> Int -> Matrix Number
zeros r c = Dense r' c' (A.replicate (r' * c') 0.0)
  where
    r' = if r > 0 then r else 1
    c' = if c > 0 then c else 1


-- | Create Matrix from Array
fromArray :: ∀ a. Int -> Int -> Array a -> Maybe (Matrix a)
fromArray r c vs | r > 0 && c > 0 && r*c == A.length vs = Just (Dense r c vs)
                 | otherwise = Nothing


-- | Get specific column as a vector. Index is 0 based
-- | If the index is out of range then return empty vector
column :: ∀ a. Int -> Matrix a -> Vector a
column c (Dense nr nc vs) = A.mapMaybe (\i -> A.index vs (i*nc+c)) (A.range 0 (nr-1))


-- | Get specific row as a vector. Index is 0 based
-- | If the index is out of range then return empty vector
row :: ∀ a. Int -> Matrix a -> Vector a
row r (Dense nr nc vs) = A.slice i j vs
  where
    i = if r >=0 && r < nr then r*nc else 0
    j = if r >=0 && r < nr then i+nc else 0


-- | Get specific element. Index is 0 based
element :: ∀ a. Int -> Int -> Matrix a -> Maybe a
element r c (Dense nr nc vs) = A.index vs ((r*nc) + c)


-- | Return list of rows
rows :: ∀ a. Matrix a -> Array (Vector a)
rows mat = do 
  i <- A.range 0 (nrows mat - 1)
  pure $ row i mat


-- | List of columns
columns :: ∀ a. Matrix a -> Array (Vector a)
columns mat = do 
  i <- A.range 0 (ncols mat - 1)
  pure $ column i mat