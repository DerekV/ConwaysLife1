module ConwaysLife
       (Universe (Universe),
        Cell (Cell),
        Position (Position),
        liveCells,
        getPosition)
       where

data Position = Position Integer Integer deriving (Eq, Show)
data Universe = Universe
data Cell = Cell Position

liveCells :: Universe -> [Cell]
liveCells _ = []

getPosition :: Cell -> Position
getPosition _ = Position 0 0
