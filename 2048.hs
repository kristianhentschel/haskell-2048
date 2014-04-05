module Main where
import Data.List
import System.Random
import Control.Monad.State

data Tile = TEmpty | TNum Int deriving (Read, Eq)
data Direction = MoveUp | MoveDown | MoveLeft | MoveRight deriving (Show, Read, Eq)
data Grid = Grid Int [[Tile]] deriving (Read, Eq)

tile_width = 6

instance Show Tile where
    show TEmpty = " " ++ (take (tile_width-2) $ repeat '_') ++ " "
    show (TNum num) = padl ++ strnum ++ padr
        where   strnum = show num
                padl = take (tile_width - length strnum - 1) $ repeat ' '
                padr = " "

instance Show Grid where
    show (Grid _ tiles) = unlines $ map concat $ map (map show) tiles

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

-- Convert the Grid into lists according to the move direction 
gridToLists :: Direction -> Grid -> [[Tile]]
gridToLists dir (Grid _ tiles) =
    case dir of
        MoveUp    -> transpose tiles
        MoveDown  -> map reverse $ transpose tiles
        MoveLeft  -> tiles
        MoveRight -> map reverse tiles

-- Restore the original Grid
listsToGrid :: Direction -> [[Tile]] -> Grid
listsToGrid dir tiles = Grid (length tiles) tiles'
    where tiles' = case dir of
            MoveUp    -> transpose tiles
            MoveDown  -> transpose $ map reverse tiles
            MoveLeft  -> tiles
            MoveRight -> map reverse tiles


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

-- user io
play :: Grid -> Bool -> StateT (StdGen) IO ()
play grid valid = do

    gen <- get
    let (rand1, gen') = next gen
    let (rand2, gen'') = next gen'
    let grid' = if valid then (addRandom grid rand1 rand2) else grid
    put gen''
    
    liftIO (putStrLn $ show grid')

    if (hasValidMoves grid') then do
        c <- liftIO getChar
        liftIO (putStrLn "")

        if (c `elem` "wasd") then do
            let grid'' = (move (dir c) grid')
            play grid'' (if grid' == grid'' then False else True)
        else do
            liftIO (putStrLn "Use your wasd keys to move the tiles. When two tiles with the same number touch, they merge into one!")
            return ()
    else do
        liftIO (putStrLn "Game Over." )
        return()
    where
        hasValidMoves g = [] /= filter (\dir -> move dir g /= g) [MoveUp, MoveDown, MoveLeft, MoveRight]
        dir c = case c of
                'w' -> MoveUp
                's' -> MoveDown
                'a' -> MoveLeft
                'd' -> MoveRight
                _   -> MoveRight
        
-- main
main :: IO ()
main = do
    putStrLn "Join the numbers and get to the 2048 tile!"
    gen <- getStdGen
    runStateT (play (emptyGrid 4) True) gen
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

