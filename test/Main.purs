module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Test.Assert (ASSERT)

import Test.LinearAlgebra.Matrix (testMatrix)
import Test.LinearAlgebra.Vector (testVector)
import Test.PerfTest (perfTests)


main :: ∀ eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
main = do
  testMatrix
  testVector
  perfTests
  