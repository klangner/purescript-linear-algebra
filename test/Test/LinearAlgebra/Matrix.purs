module Test.LinearAlgebra.Matrix (testMatrix) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Maybe (Maybe(..), fromJust, isJust, isNothing)
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

    log "Get specific row"
    assert $ M.row 0 z2 == [1.0, 2.0, 3.0]    

    log "Get specific column"
    assert $ M.column 1 z2 == [2.0, 5.0]

    log "Get specific value"
    assert $ M.element 1 2 z2 == Just 6.0

    