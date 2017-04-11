module Test.LinearAlgebra.Matrix (testMatrix) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Maybe (fromJust, isJust, isNothing)
import Test.Assert (assert, ASSERT)
import Partial.Unsafe (unsafePartial)
import LinearAlgebra.Matrix as M


testMatrix :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testMatrix = do

    log "Init Matrix test case data"
    let z1 = unsafePartial $ fromJust $ M.zeros 5 4
    let z2 = unsafePartial $ fromJust $ M.fromArray 2 3 [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]

    log "Create matrix"
    assert $ z1.nrows == 5
    assert $ z1.ncols == 4

    log "From Array"
    assert $ isNothing (M.fromArray 2 3 [1.0])
    assert $ isJust (M.fromArray 2 3 [1.0, 2.0, 3.0, 4.0, 5.0, 6.0])