module Main where
import Data.List
import System.Random
import Control.Monad.State

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

-- Create an empty Grid for the start of the game
emptyGrid :: Int -> Grid
emptyGrid size = Grid size tiles
    where tiles = take size $ repeat $ take size $ repeat TEmpty

-- Get coordinates of the empty grid positions
getEmptyPositions :: Grid -> [(Int, Int)]
getEmptyPositions (Grid size tiles) = 
    let
        flattiles = concat tiles
        positions = concat $ map (\x -> zip (repeat x) [0..(size-1)]) [0..(size-1)]
    in
        [pos | (tile, pos) <- (zip flattiles positions), tile == TEmpty]

-- replace a random empty tile in the grid with a 2 (90%) or 4 (10%) tile
addRandom :: Grid -> Int -> Int -> Grid
addRandom grid rand1 rand2 =
    let
        Grid size tiles = grid
        randTile = TNum (if rand1 `mod` 100 <= 90 then 2 else 4)
        empties  = getEmptyPositions grid
        randPos  = empties !! (rand2 `mod` (length empties))
        tiles'   = map (\(x, row) ->
            map (\(y, t) ->
                if (x,y) == randPos then randTile else t )
                $ zip [0..size-1] row)
            $ zip [0..size-1] tiles
    in
        Grid size tiles'

-- Apply a move in the given direction to the grid
move :: Direction -> Grid -> Grid
move dir grid = ((listsToGrid dir) . (map moveList) . (gridToLists dir)) grid

-- TODO so far assuming all player moves are valid.
-- user io
play :: Grid -> StateT (StdGen) IO ()
play grid = do
    gen <- get
    let (rand1, gen') = next gen
    let (rand2, gen'') = next gen'
    put gen''
    let grid' = addRandom grid rand1 rand2
    
    liftIO (putStrLn $ show grid)
    c <- liftIO getChar
    liftIO (putStrLn "")

    case c of
        'w' -> play $ move MoveUp       grid'
        's' -> play $ move MoveDown     grid'
        'a' -> play $ move MoveLeft     grid'
        'd' -> play $ move MoveRight    grid'
        _   -> return()
        
-- main
main :: IO ()
main = do
    putStrLn "=== 2048 ==="
    runStateT (play (emptyGrid 4)) (mkStdGen 0)
    return ()

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

