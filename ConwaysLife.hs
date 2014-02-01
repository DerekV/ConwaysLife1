module ConwaysLife
       (Universe (Universe),
        Position (Position),
        emptyUniverse,
        liveCells,
        isCellAlive,
        setCellAlive,
        nextGen)
       where

import Control.Applicative
import  qualified Data.MultiSet as MS

data Position = Position Integer Integer deriving (Eq, Show, Ord)
data Universe = Universe [Position]

emptyUniverse :: Universe
emptyUniverse = Universe []

liveCells :: Universe -> [Position]
liveCells (Universe cells) = cells

isCellAlive :: Position -> Universe -> Bool
isCellAlive pos (Universe cells) = elem pos cells

setCellAlive :: Position -> Universe -> Universe
setCellAlive pos (Universe cells) = Universe (pos : cells)

nextGen :: Universe -> Universe
nextGen (Universe oldCells) =
  Universe $ filterLive cellCounts
  where
    cellCounts = countAllTheThings $ map enumerateNeighbors oldCells

countAllTheThings :: Ord a => [[a]] -> MS.MultiSet a
countAllTheThings originals =
  MS.unions $ map (MS.fromList) originals

enumerateNeighbors :: Position -> [Position]
enumerateNeighbors (Position x y) =
  Position <$> [x-1 .. x+1] <*> [y-1 .. y+1]

filterLive :: MS.MultiSet Position -> [Position]
filterLive mSet = map fst $ filter ((==3).snd) $ MS.toOccurList mSet
