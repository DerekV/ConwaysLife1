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
tests =
  [
    testGroup "Conways Game Of Life"
    [
      testGroup "Universe (Model tests)"
      [
        testCase "A empty universe is created with no live cells"
        ( assert $ null $ liveCells emptyUniverse),
        testCase "A universe seeded with some cells has those cells as alive"
        ( liveCells (Universe [Position 0 1, Position 2 2 ])
          @?= [Position 0 1, Position 2 2]),
        testGroup "A position is defined by an ordered pair of integers"
        [
          testCase "origin = origin" ((Position 0 0) @=? (Position 0 0)),
          testCase "42,-7 = 42,-7"   ((Position 42 (-7)) @=? (Position 42 (-7))),
          testCase "mirrored not equal"
          ( assert $ not $ (Position 42 (-7)) == (Position 42 7))
        ],
        testGroup "Each position in universe might have living cell"
        [
          testProperty "Newly created universe means all positions nonliving"
          ( pAllCellsDead emptyUniverse ),
          testCase "We can create a universe that only has a living cell at origin"
          ( assert
            $ isCellAlive (Position 0 0)
            $ setCellAlive (Position 0 0) emptyUniverse ),
          testCase "Regression, setting -2 1 as alive"
          ( assert
            $ isCellAlive (Position (-2) 1)
            $ setCellAlive (Position (-2) 1) emptyUniverse ),
          testProperty "If i set a cell as alive, that cell is indeed alive when I look at"
          ( pCheckCellSetAlive )
        ]
      ],
      testGroup "Engine tests"
      [
        testCase "One cell at origin to no live cells"
        ( assert $ null $ liveCells
          ( nextGen $ setCellAlive (Position 0 0) emptyUniverse )),
        testCase "Three seperated cells around origin to one at origin"
        ( assert
          (
            (liveCells $
             nextGen $
             setCellAlive (Position 0 1) $
             setCellAlive (Position (-1) (-1)) $
             setCellAlive (Position 1 (-1)) emptyUniverse
            )
            ==
            (liveCells $ setCellAlive (Position 0 0) emptyUniverse )
          )
        ),
        testCase "Three vertically stacked yeilds three horizontally stacked"
        ( assert
          ((liveCells $ nextGen $
            setCellAlive (Position 0 1) $
            setCellAlive (Position 0 0) $
            setCellAlive (Position 0 (-1)) emptyUniverse)
           ==
           [(Position (-1) 0), (Position 0 0), (Position 1 0)]))
      ]
    ]
  ]



pAllCellsDead :: Universe -> Position -> Bool
pAllCellsDead universe position =
  not $ isCellAlive position universe

pCheckCellSetAlive :: Position -> Bool
pCheckCellSetAlive pos =
  let newUniverse = setCellAlive pos emptyUniverse
      in
   isCellAlive pos newUniverse

main :: IO ()
main = defaultMain tests
