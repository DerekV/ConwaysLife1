module ConwaysLife (Universe (Universe), Cell (Cell), liveCells) where

data Universe = Universe
data Cell = Cell deriving (Show, Eq)

liveCells :: Universe -> [Cell]
liveCells _ = []
