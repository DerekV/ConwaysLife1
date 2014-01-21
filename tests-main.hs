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
    (assert $ null $ liveCells (Universe))
  ]
        ]


main :: IO ()
main = defaultMain tests
