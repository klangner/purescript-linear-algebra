module Test.LinearAlgebra.Vector (testVector) where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)
import Data.Maybe (Maybe(..))
import Test.Assert (assert)

import LinearAlgebra.Vector as V


testVector :: Effect Unit
testVector = do
    log "\n# Vector tests"
    testAdd
    testDifference
    testDot
    testMulScalar
    testArgMax
    testtestArgMin


testAdd :: Effect Unit
testAdd = do
    log " * Add vectors"
    assert $ V.add [1.0, 2.0, -3.0] [1.0, 1.0, 1.0, 5.0] == []
    assert $ V.add [1.0, 2.0, -3.0] [1.0, 1.0, 1.0] == [2.0, 3.0, -2.0]


testDifference :: Effect Unit
testDifference = do
    log " * Difference"
    assert $ V.diff [1.0, 2.0, -3.0] [1.0, 1.0, 1.0, 5.0] == []
    assert $ V.diff [1.0, 2.0, -3.0] [1.0, 1.0, 1.0] == [0.0, 1.0, -4.0]


testDot :: Effect Unit
testDot = do
    log " * Dot product"
    assert $ V.dot [1.0, 2.0, -3.0] [1.0, 2.0, 1.0] == Just 2.0


testMulScalar :: Effect Unit
testMulScalar = do
    log " * Multiply by scalar"
    assert $ V.mulScalar 2.0 [1.0, 2.0, -3.0] ==  [2.0, 4.0, -6.0]


testArgMax :: Effect Unit
testArgMax = do
    log " * argmax"
    assert $ V.argmax [1.0, 2.0, -3.0, 1.0, 2.0, 1.0] == 1


testtestArgMin :: Effect Unit
testtestArgMin = do
    log " * argmin"
    assert $ V.argmin [1.0, 2.0, -3.0, 1.0, 2.0, 1.0] == 2
        
