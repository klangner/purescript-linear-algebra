module LinearAlgebra.Vector
  ( Vector
  , add
  , argmax
  , argmin
  , diff
  , dot
  , mulScalar
  , sum
  ) where

import Prelude
import Global (infinity)
import Data.Array as A
import Data.Maybe (Maybe(..))  
import Data.Tuple (Tuple(..), fst, snd)


-- | Dense Vector implementation
type Vector = Array
  

-- | Difference between 2 vectors.
-- | Vector have to have the same size
diff :: Vector Number -> Vector Number -> Vector Number
diff xs ys 
  | (A.length xs) /= (A.length ys) = []
  | otherwise = A.zipWith (-) xs ys


-- | Add 2 vectors.
-- | Vector have to have the same size
add :: ∀ a. Semiring a => Vector a -> Vector a -> Vector a
add xs ys 
  | (A.length xs) /= (A.length ys) = []
  | otherwise = A.zipWith (+) xs ys


-- | Dot product between 2 vectors
-- | https://en.wikipedia.org/wiki/Dot_product
dot :: ∀ a. Semiring a => Vector a -> Vector a -> Maybe a
dot xs ys 
  | (A.length xs) /= (A.length ys) = Nothing
  | otherwise = Just $ sum (A.zipWith (*) xs ys)


-- | Sum vector elements
sum :: ∀ a. Semiring a => Vector a -> a
sum xs = A.foldl (+) zero xs


-- | Return index of the first maximum value
-- | This function will return 0 index for empty vector
argmax :: Vector Number -> Int 
argmax vs = fst $ A.foldl f (Tuple 0 (-infinity)) xs
  where 
    xs = A.mapWithIndex Tuple vs
    f tu tv = if (snd tv) > (snd tu) then tv else tu


-- | Return index of the first minimum value
-- | This function will return 0 index for empty vector
argmin :: Vector Number -> Int 
argmin vs = fst $ A.foldl f (Tuple 0 infinity) xs
  where 
    xs = A.mapWithIndex Tuple vs
    f tu tv = if (snd tv) < (snd tu) then tv else tu


-- | Multiply vector by scalar
mulScalar :: Number -> Vector Number -> Vector Number 
mulScalar d vs = map (_ * d) vs    