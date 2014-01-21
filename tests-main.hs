module Main (main) where

import Control.Applicative
import Control.Monad
import Test.HUnit
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck
import ConwaysLife

instance Test.QuickCheck.Arbitrary Position where
  arbitrary = liftM2 Position arbitrary arbitrary

tests :: [Test.Framework.Test]
tests = [
  testGroup "Universe (Model tests)"
  [
    testCase "A empty universe is created with no live cells"
    (assert $ null $ liveCells emptyUniverse),
    testGroup "A position is defined by an ordered pair of integers"
    [
      testCase "origin = origin" ((Position 0 0) @=? (Position 0 0)),
      testCase "42,-7 = 42,-7"   ((Position 42 (-7)) @=? (Position 42 (-7))),
      testCase "mirrored not equal"
      (assert $ not $ (Position 42 (-7)) == (Position 42 7))
    ],
    testGroup "Each position in universe might have living cell"
    [
      testProperty "Newly created universe means all positions nonliving"
      ( pAllCellsDead emptyUniverse ),
      testCase "We can create a universe that only has a living cell at origin"
      ( assert
        $ isCellAlive (Position 0 0)
        $ setCellAlive (Position 0 0) emptyUniverse )
    ]
  ]]

pAllCellsDead :: Universe -> Position -> Bool
pAllCellsDead universe position =
  not $ isCellAlive position universe

main :: IO ()
main = defaultMain tests
