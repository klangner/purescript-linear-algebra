module Test.Main where

import Prelude
import Effect (Effect)

import Test.LinearAlgebra.Matrix (testMatrix)
import Test.LinearAlgebra.Vector (testVector)
import Test.PerfTest (perfTests)


main :: Effect Unit
main = do
  testMatrix
  testVector
  perfTests
  
