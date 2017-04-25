module Test.LinearAlgebra.Vector (testVector) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Maybe (Maybe(..))
import Test.Assert (assert, ASSERT)

import LinearAlgebra.Vector as V


testVector :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testVector = do

    log "\n#Vector tests"

    log "Add vectors"
    assert $ V.add [1.0, 2.0, -3.0] [1.0, 1.0, 1.0, 5.0] == Nothing
    assert $ V.add [1.0, 2.0, -3.0] [1.0, 1.0, 1.0] == Just [2.0, 3.0, -2.0]

    log "Difference"
    assert $ V.diff [1.0, 2.0, -3.0] [1.0, 1.0, 1.0, 5.0] == Nothing
    assert $ V.diff [1.0, 2.0, -3.0] [1.0, 1.0, 1.0] == Just [0.0, 1.0, -4.0]

    log "Dot product"
    assert $ V.dot [1.0, 2.0, -3.0] [1.0, 2.0, 1.0] == Just (2.0)

    log "argmax"
    assert $ V.argmax [1.0, 2.0, -3.0, 1.0, 2.0, 1.0] == 1

    log "argmin"
    assert $ V.argmin [1.0, 2.0, -3.0, 1.0, 2.0, 1.0] == 2
        