module Main (main) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import ConwaysLife

tests :: [Test.Framework.Test]
tests = [
  testGroup "Universe (Model tests)"
  [
    testCase "A universe is created with no live cells"
    (assert $ null $ liveCells (Universe)),
    testCase "A cell has a position, position has identity and equivlence"
    (let position = (Position 0 0) in
      getPosition (Cell position) @=? position ),
    testGroup "A position is defined by an ordered pair of integers"
    [
      testCase "origin = origin" ((Position 0 0) @=? (Position 0 0)),
      testCase "42,-7 = 42,-7"   ((Position 42 (-7)) @=? (Position 42 (-7))),
      testCase "mirrored not equal"
      (assert $ not $ (Position 42 (-7)) == (Position 42 7))
    ]
  ]]


main :: IO ()
main = defaultMain tests
