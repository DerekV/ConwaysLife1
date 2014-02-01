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
data Universe = Universe (Set.Set Position)

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
  Universe $ filterLive cellCounts
  where
    cellCounts = countAllTheThings $ neighborsList
    neighborsList = map enumerateNeighbors $ Set.toList oldCells

countAllTheThings :: Ord a => [[a]] -> MS.MultiSet a
countAllTheThings originals =
  MS.unions $ map (MS.fromList) originals

enumerateNeighbors :: Position -> [Position]
enumerateNeighbors (Position x y) =
  Position <$> [x-1 .. x+1] <*> [y-1 .. y+1]

filterLive :: MS.MultiSet Position -> Set.Set Position
filterLive mSet = Set.fromList $
                  map fst $ filter ((==3).snd) $ MS.toOccurList mSet
