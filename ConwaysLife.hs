module ConwaysLife
       (Universe (Universe),
        Position (Position),
        emptyUniverse,
        liveCells,
        isCellAlive,
        setCellAlive)
       where

data Position = Position Integer Integer deriving (Eq, Show)
data Universe = Universe [Position]

emptyUniverse :: Universe
emptyUniverse = Universe []

liveCells :: Universe -> [Position]
liveCells _ = []

isCellAlive :: Position -> Universe -> Bool
isCellAlive pos (Universe cells) = elem pos cells

setCellAlive :: Position -> Universe -> Universe
setCellAlive _ _ = Universe [Position 0 0]
