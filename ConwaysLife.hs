module ConwaysLife
       (Universe (Universe),
        Cell (Cell),
        Position (Position),
        emptyUniverse,
        liveCells,
        getPosition,
        isCellAlive,
        setCellAlive)
       where

data Position = Position Integer Integer deriving (Eq, Show)
data Universe = Universe [Cell]
data Cell = Cell Position deriving (Eq)

emptyUniverse :: Universe
emptyUniverse = Universe []

liveCells :: Universe -> [Cell]
liveCells _ = []

getPosition :: Cell -> Position
getPosition _ = Position 0 0

isCellAlive :: Position -> Universe -> Bool
isCellAlive pos (Universe cells) = elem (Cell pos) cells

setCellAlive :: Position -> Universe -> Universe
setCellAlive _ _ = Universe [Cell $ Position 0 0]
