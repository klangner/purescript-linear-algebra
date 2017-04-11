module LinearAlgebra.Matrix 
  ( Matrix 
  , replicate
  , zeros
  ) where

import Prelude
import Data.Array as A 


-- | Dense Matrix implementation
type Matrix a = 
  { nrows :: Int 
  , ncols :: Int 
  , values :: Array a 
  }


-- | Create array of given dimmension containing replicated value
replicate :: ∀ a. Int -> Int -> a -> Matrix a 
replicate r c v = {nrows: r, ncols: c, values: vs}
  where 
    vs = A.replicate (r * c) v


-- | Create array of given dimmension with all values set to 0
zeros :: Int -> Int -> Matrix Number
zeros r c = replicate r c 0.0
