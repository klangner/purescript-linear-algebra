module LinearAlgebra.Matrix 
  ( Matrix 
  ) where
  

-- | Dense Matrix implementation
type Matrix a = 
  { nrows :: Int 
  , ncols :: Int 
  , data :: Array a 
  }


-- -- | Create Matrix from array of values
-- fromArray :: ∀ a. Array a -> Int -> Int -> Maybe (Matrix a)
-- fromArray ds rows cols | length ds