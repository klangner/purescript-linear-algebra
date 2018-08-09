module Test.PerfTest (perfTests) where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)
import Data.Array as A
import Data.Maybe (fromJust)
import Test.Assert (assert)
import Partial.Unsafe (unsafePartial)
import LinearAlgebra.Matrix as M


perfTests :: Effect Unit
perfTests = do

    log "\n# Run Benchmarks"

    log "\nGet columns"
    let a1 = unsafePartial $ fromJust $ M.replicate 1000000 1 1.0
    let cs = M.columns a1
    log $ "There are " <> show (A.length cs) <> " columns."
    
    log "\nGet rows"
    let a2 = unsafePartial $ fromJust $ M.replicate 10 1000000 1.0
    let rs = M.rows a2
    log $ "There are " <> show (A.length rs) <> " rows."

    assert true
