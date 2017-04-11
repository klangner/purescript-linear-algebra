module Test.LinearAlgebra.Matrix (testMatrix) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Test.Assert (assert, ASSERT)
import LinearAlgebra.Matrix as M


testMatrix :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testMatrix = do

    log "Init Matrix test case data"
    let z1 = M.zeros 5 4

    log "Create zero matrix"
    assert $ z1.nrows == 5
    assert $ z1.ncols == 4
