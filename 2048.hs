module Main where
import Data.List

data Tile = TEmpty | TNum Int deriving (Show, Read, Eq)
data Direction = MoveUp | MoveDown | MoveLeft | MoveRight deriving (Show, Read, Eq)
data Grid = Grid Int [[Tile]] deriving (Show, Read, Eq)

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

-- Transpose the Grid and reverse lists as required, so each row starts with the tile we are moving towards.
transposeTiles :: Direction -> [[Tile]] -> [[Tile]]
transposeTiles dir tiles = case dir of
    MoveUp    -> transpose tiles
    MoveDown  -> map reverse $ transpose tiles
    MoveLeft  -> tiles
    MoveRight -> map reverse tiles

-- Convert the Grid into lists according to the move direction 
gridToLists :: Direction -> Grid -> [[Tile]]
gridToLists dir (Grid _ tiles) = transposeTiles dir tiles

-- Restore the original Grid
listsToGrid :: Direction -> [[Tile]] -> Grid
listsToGrid dir tiles = Grid (length tiles) tiles'
    where tiles' = transposeTiles dir tiles

-- Create an empty grid for the start of the game
emptyGrid :: Int -> Grid
emptyGrid size = Grid size tiles
    where tiles = take size $ repeat $ take size $ repeat TEmpty

-- replace a random empty tile in the grid with a 2 (90%) or 4 (10%) tile
addRandom :: Grid -> Grid
addRandom grid = grid

-- Do the move, and if the grid changed, check if the game is lost or add a random new tile.
move :: Direction -> Grid -> Grid
move dir grid = grid'
    where grid' = ((listsToGrid dir) . (map moveList) . (gridToLists dir)) grid

-- user io
main :: IO ()
main = return()







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

