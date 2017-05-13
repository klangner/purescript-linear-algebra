module Test.LinearAlgebra.Matrix (testMatrix) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Test.Assert (assert, ASSERT)

import LinearAlgebra.Matrix as M


testMatrix :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testMatrix = do
    log "\n# Test Matrix"
    testCreate
    testFromArray
    testGetRow
    testRows
    testGetValue
    testGetCol
    testColumns
    testSliceColumns
    testSliceRows
    testInsertColumn
    testMultiply
    testAdd
    testTranspose
    testIdentity


testCreate :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testCreate = do
    log " * Create matrix"
    let z1 = M.zeros 5 4
    assert $ M.nrows z1 == 5
    assert $ M.ncols z1 == 4


testFromArray :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testFromArray = do
    log " * From Array"
    assert $ isNothing (M.fromArray 2 3 [1.0])
    assert $ isJust (M.fromArray 2 3 [1.0, 2.0, 3.0, 4.0, 5.0, 6.0])


testGetRow :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testGetRow = do
    log " * Get specific row"
    let mat = fromMaybe (M.zeros 1 1) $ M.fromArray 2 3 [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
    assert $ M.row 0 mat == [1.0, 2.0, 3.0]    


testGetCol :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testGetCol = do
    log " * Get specific column"
    let mat = fromMaybe (M.zeros 1 1) $ M.fromArray 2 3 [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
    assert $ M.column 1 mat == [2.0, 5.0]


testGetValue :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testGetValue = do
    log " * Get specific value"
    let mat = fromMaybe (M.zeros 1 1) $ M.fromArray 2 3 [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
    assert $ M.element 1 2 mat == Just 6.0


testRows :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testRows = do
    log " * Get list of rows"
    let mat = fromMaybe (M.zeros 1 1) $ M.fromArray 2 3 [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
    assert $ M.rows mat == [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]
    

testColumns :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testColumns = do
    log " * Get list of columns"
    let mat = fromMaybe (M.zeros 1 1) $ M.fromArray 2 3 [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
    assert $ M.columns mat == [[1.0, 4.0], [2.0, 5.0], [3.0, 6.0]]
    

testSliceColumns :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testSliceColumns = do
    log " * Slice columns"
    let mat = fromMaybe (M.zeros 1 1) $ M.fromArray 2 3 [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
    let expected = fromMaybe (M.zeros 1 1) $ M.fromArray 2 2 [1.0, 2.0, 4.0, 5.0]
    assert $ M.sliceCols 0 1 mat == expected
    

testSliceRows :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testSliceRows = do
    log " * Slice rows"
    let mat = fromMaybe (M.zeros 1 1) $ M.fromArray 3 2 [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
    let expected = fromMaybe (M.zeros 1 1) $ M.fromArray 2 2 [1.0, 2.0, 3.0, 4.0]
    assert $ M.sliceRows 0 1 mat == expected
    

testInsertColumn :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testInsertColumn = do
    log " * Insert column"
    let mat = fromMaybe (M.zeros 1 1) $ M.fromArray 2 2 [1.0, 2.0, 4.0, 5.0]
    let expected1 = fromMaybe (M.zeros 1 1) $ M.fromArray 2 3 [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
    let expected2 = fromMaybe (M.zeros 1 1) $ M.fromArray 2 3 [3.0, 1.0, 2.0, 6.0, 4.0, 5.0]
    assert $ M.insertCol 2 [3.0, 6.0] mat == expected1
    assert $ M.insertCol 0 [3.0, 6.0] mat == expected2


testAdd :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testAdd = do
    log " * Multiply matrices"
    let m1 = fromMaybe (M.zeros 1 1) $ M.fromArray 2 3 [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
    let expected = fromMaybe (M.zeros 1 1) $ M.fromArray 2 3 [2.0, 4.0, 6.0, 8.0, 10.0, 12.0]
    assert $ M.add m1 m1 == expected


testMultiply :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testMultiply = do
    log " * Multiply matrices"
    let m1 = fromMaybe (M.zeros 1 1) $ M.fromArray 2 3 [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
    let m2 = fromMaybe (M.zeros 1 1) $ M.fromArray 3 2 [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
    let expected = fromMaybe (M.zeros 1 1) $ M.fromArray 2 2 [22.0, 28.0, 49.0, 64.0]
    assert $ M.multiply m1 m2 == expected


testTranspose :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testTranspose = do
    log " * Transpose matrix"
    let mat = fromMaybe (M.zeros 1 1) $ M.fromArray 2 3 [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
    let expected = fromMaybe (M.zeros 1 1) $ M.fromArray 3 2 [1.0, 4.0, 2.0, 5.0, 3.0, 6.0]
    assert $ M.transpose mat == expected


testIdentity :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testIdentity = do
    log " * Identity matrix"
    let expected = fromMaybe (M.zeros 1 1) $ M.fromArray 3 3 [1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0]
    assert $ M.identity 3 == expected
