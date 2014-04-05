module Main where


data Position = Position Integer Integer deriving (Show, Read)
data Tile = TEmpty | TNum Integer deriving (Show, Read, Eq)
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

-- Apply the 2048 rules to a single list of tiles representing a row or column, moving towards the first element.
moveList :: [Tile] -> [Tile]
moveList []             = []
moveList (TEmpty:xs)    = moveList xs ++ [TEmpty]
moveList (x:TEmpty:xs)  = moveList (x:xs) ++ [TEmpty]
moveList (x:y:xs)       = 
    if (x == y) then
        double:moveList(xs) ++ [TEmpty]
    else
        x:moveList(y:xs)
    where double = TNum (numx * 2)
          TNum numx = x
moveList (x:xs)         = (x:xs)

-- some test cases for manual inspection in ghci. TODO port to HUnit or similar.
testMoveList :: ([Tile] -> [Tile]) -> [[Tile]]
testMoveList f =
    let tests = [
            [TEmpty, TNum 2, TEmpty, TEmpty],
            [TNum 2, TEmpty, TEmpty, TEmpty],
            [TNum 2, TNum 2, TEmpty, TEmpty],
            [TNum 2, TEmpty, TNum 2, TEmpty],
            [TNum 2, TEmpty, TEmpty, TNum 2],
            [TNum 2, TNum 2, TNum 2, TEmpty],
            [TNum 2, TNum 2, TNum 2, TNum 2]]
    in
        map f tests

-- Convert the Grid into a list of columns or rows, starting with the edge moved towards
gridToLists :: Direction -> Grid -> [[Tile]]
gridToLists dir grid = [[]]

-- Restore the Grid from the lists of Tiles
listsToGrid :: Direction -> [[Tile]] -> Grid
listsToGrid dir tiles = Grid 0 []

-- replace a random empty tile in the grid with a 2 (90%) or 4 (10%) tile
addRandom :: Grid -> Grid
addRandom grid = grid

-- Do the move, and if the grid changed, check if the game is lost or add a random new tile.
move :: Grid -> Direction -> Grid
move grid dir = grid'
    where grid' = ((listsToGrid dir). (map moveList) . (gridToLists dir)) grid

-- user io
main :: IO ()
main = return()
