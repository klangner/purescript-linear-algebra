module LinearAlgebra.Matrix 
  ( Matrix 
  -- * Constructors
  , fromArray
  , replicate
  , zeros
  -- * Access data
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
import Data.Tuple (Tuple, fst, snd)
import LinearAlgebra.Vector (Vector)


-- | Dense Matrix implementation
type Matrix a = 
  { nrows :: Int 
  , ncols :: Int 
  , values :: Array a 
  }


-- | Create array of given dimmension containing replicated value
replicate :: ∀ a. Int -> Int -> a -> Maybe (Matrix a )
replicate r c v | r > 0 && c > 0 = Just {nrows: r, ncols: c, values: A.replicate (r * c) v}
                | otherwise = Nothing


-- | Create array of given dimmension with all values set to 0
zeros :: Int -> Int -> Maybe (Matrix Number)
zeros r c = replicate r c 0.0


-- | Create Matrix from Array
fromArray :: ∀ a. Int -> Int -> Array a -> Maybe (Matrix a)
fromArray r c vs | r > 0 && c > 0 && r*c == A.length vs = Just {nrows: r, ncols: c, values: vs}
                 | otherwise = Nothing


-- | Get specific column as a vector. Index is 0 based
-- | If the index is out of range then return empty vector
column :: ∀ a. Int -> Matrix a -> Vector a
column c mat = snd <<< A.unzip $ A.filter isInColumn ivalues
  where
    ivalues :: Array (Tuple Int a)
    ivalues = A.zip (A.range 0 (A.length mat.values - 1)) mat.values

    isInColumn :: forall b. Tuple Int b -> Boolean
    isInColumn v = (fst v) `mod` mat.ncols == c



-- | Get specific row as a vector. Index is 0 based
-- | If the index is out of range then return empty vector
row :: ∀ a. Int -> Matrix a -> Vector a
row r mat = A.slice i j mat.values
  where
    i = if r >=0 && r < mat.nrows then r*mat.ncols else 0
    j = if r >=0 && r < mat.nrows then i+mat.ncols else 0


-- | Get specific element. Index is 0 based
element :: ∀ a. Int -> Int -> Matrix a -> Maybe a
element r c mat = A.index mat.values ((r*mat.ncols) + c)


-- | Return list of rows
rows :: ∀ a. Matrix a -> Array (Vector a)
rows mat = do 
  i <- A.range 0 (mat.nrows - 1)
  pure $ row i mat


-- | List of columns
columns :: ∀ a. Matrix a -> Array (Vector a)
columns mat = do 
  i <- A.range 0 (mat.ncols - 1)
  pure $ column i mat