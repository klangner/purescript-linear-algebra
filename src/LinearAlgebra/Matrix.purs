module LinearAlgebra.Matrix 
  ( Matrix 
  , fromArray
  , replicate
  , zeros
  ) where

import Prelude
import Data.Array as A 
import Data.Maybe (Maybe(..))


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