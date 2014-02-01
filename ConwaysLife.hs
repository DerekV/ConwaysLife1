module ConwaysLife
       (Universe,
        Position (Position),
        emptyUniverse,
        universeWithLiveCells,
        liveCells,
        isCellAlive,
        setCellAlive,
        nextGen)
       where

import Control.Applicative
import qualified Data.MultiSet as MS
import qualified Data.Set as Set

data Position = Position Integer Integer deriving (Eq, Show, Ord)
data Universe = Universe (Set.Set Position) deriving (Eq, Show)

emptyUniverse :: Universe
emptyUniverse = Universe Set.empty

liveCells :: Universe -> [Position]
liveCells (Universe cells) = Set.toList cells

isCellAlive :: Position -> Universe -> Bool
isCellAlive pos (Universe cells) = Set.member pos cells

setCellAlive :: Position -> Universe -> Universe
setCellAlive pos (Universe cells) = Universe (Set.insert pos cells)

universeWithLiveCells :: [Position] -> Universe
universeWithLiveCells cellList = Universe $ Set.fromList cellList

nextGen :: Universe -> Universe
nextGen (Universe oldCells) =
  Universe $ determineLive oldCells cellCounts
  where
    cellCounts = countAllTheThings $ neighborsList
    neighborsList = map enumerateNeighbors $ Set.toList oldCells

countAllTheThings :: Ord a => [[a]] -> MS.MultiSet a
countAllTheThings originals =
  MS.unions $ map (MS.fromList) originals

enumerateNeighbors :: Position -> [Position]
enumerateNeighbors (Position x y) =
  Position <$> [x-1 .. x+1] <*> [y-1 .. y+1]

determineLive :: Set.Set Position -> MS.MultiSet Position -> Set.Set Position
determineLive oldCells mSet = Set.fromList $
                  map fst $ filter cellRule $
                  MS.toOccurList mSet
                  where
                    cellRule (pos, cnt) =
                      (if (Set.member pos oldCells)
                         then liveCellRule cnt
                         else deadCellRule cnt)
                    liveCellRule x = x==3 || x==4
                    deadCellRule x = x==3
