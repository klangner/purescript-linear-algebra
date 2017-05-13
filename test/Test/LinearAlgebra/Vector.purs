module Test.LinearAlgebra.Vector (testVector) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Maybe (Maybe(..))
import Test.Assert (assert, ASSERT)

import LinearAlgebra.Vector as V


testVector :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testVector = do
    log "\n# Vector tests"
    testAdd
    testDifference
    testDot
    testMulScalar
    testArgMax
    testtestArgMin


testAdd :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testAdd = do
    log " * Add vectors"
    assert $ V.add [1.0, 2.0, -3.0] [1.0, 1.0, 1.0, 5.0] == []
    assert $ V.add [1.0, 2.0, -3.0] [1.0, 1.0, 1.0] == [2.0, 3.0, -2.0]


testDifference :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testDifference = do
    log " * Difference"
    assert $ V.diff [1.0, 2.0, -3.0] [1.0, 1.0, 1.0, 5.0] == []
    assert $ V.diff [1.0, 2.0, -3.0] [1.0, 1.0, 1.0] == [0.0, 1.0, -4.0]


testDot :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testDot = do
    log " * Dot product"
    assert $ V.dot [1.0, 2.0, -3.0] [1.0, 2.0, 1.0] == Just 2.0


testMulScalar :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testMulScalar = do
    log " * Multiply by scalar"
    assert $ V.mulScalar 2.0 [1.0, 2.0, -3.0] ==  [2.0, 4.0, -6.0]


testArgMax :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testArgMax = do
    log " * argmax"
    assert $ V.argmax [1.0, 2.0, -3.0, 1.0, 2.0, 1.0] == 1


testtestArgMin :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testtestArgMin = do
    log " * argmin"
    assert $ V.argmin [1.0, 2.0, -3.0, 1.0, 2.0, 1.0] == 2
        