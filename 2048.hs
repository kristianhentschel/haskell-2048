module Main where


data Position = Position Integer Integer deriving (Show, Read)
data Tile = TEmpty | TNum Integer deriving (Show, Read)
data Direction = MoveUp | MoveDown | MoveLeft | MoveRight
data Grid = Grid Integer [(Position, Tile)]

-- Find the position moved towards for the given position.
adjacentPos :: Grid -> Position -> Direction -> Maybe Position
adjacentPos (Grid size _) (Position x y) dir = 
    let
        (x', y') = case dir of
            MoveUp      -> (x,     y - 1)
            MoveDown    -> (x,     y + 1)
            MoveLeft    -> (x - 1, y)
            MoveRight   -> (x + 1, y)
    in
        if (x < size && x >= 0 && y < size && y >= 0) then Just $ Position x' y' else Nothing

-- Convert the Grid into a list of columns or rows, starting with the edge moved towards
gridToLists :: Grid -> Direction -> [[Tile]]
gridToLists grid dir = [[]]

-- Restore the Grid from the lists of Tiles
listsToGrid :: [[Tile]] -> Direction -> Grid
listsToGrid tiles dir = Grid 0 []

-- replace a random empty tile in the grid with a 2 (90%) or 4 (10%) tile
addRandom :: Grid -> Grid
addRandom grid = grid

-- Do the move, and if the grid changed, check if the game is lost or add a random new tile.
move :: Grid -> Direction -> Grid
move grid dir = grid

-- user io
main :: IO ()
main = return()
