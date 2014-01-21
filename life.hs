module ConwaysLife (Universe (Universe), Cell (Cell), liveCells) where

data Universe = Universe
data Cell = Cell

liveCells :: Universe -> [Cell]
liveCells _ = []
