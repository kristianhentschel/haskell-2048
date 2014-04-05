module Main where


data Position = Position Integer Integer deriving (Show, Read)
data Tile = TEmpty | TNum Integer deriving (Show, Read)
data Direction = MoveUp | MoveDown | MoveLeft | MoveRight
data Grid = Grid Integer [(Position, Tile)]

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

main :: IO ()
main = return()
