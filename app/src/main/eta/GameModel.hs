module GameModel ( emptyBoard
                 , initialGameState
                 , rotateBoard
                 , setTile
                 , tileToInt
                 , intToTile
                 , tilesWithCoordinates
                 , readTile
                 ) where

import System.Random (StdGen)
import Data.List (transpose)

import Types

-- | Given a Board we return a tile which can be found on a given row and
-- column
readTile :: (Row, Column) -> Board -> Tile
readTile (row, column) (Board b) = (b !! row) !! column

-- | Set tile on a given board to a given row and column
setTile :: (Row, Column) -> Board -> Tile -> Board
setTile (row, column) (Board b) tile =
    let r = b !! row
        nr = take column r ++ [tile] ++ drop (column + 1) r
    in Board $ take row b ++ [nr] ++ drop (row + 1) b

-- | Convert a tile to the int it represents. Empty tile is treated like 0
tileToInt :: Tile -> Int
tileToInt tile = case tile of
                   Number v -> v
                   Empty -> 0

-- | Convert an int into a tile representing it. 0 is treated like Empty
-- tile
intToTile :: Int -> Tile
intToTile n = case n of
                0 -> Empty
                _ -> Number n

-- | Convert a board into a list of all tiles with their respective
-- coordinates
tilesWithCoordinates :: Board -> [(Tile, Row, Column)]
tilesWithCoordinates (Board b) = concat
                               $ zipWith (\rowIndex row -> fmap (\(tile, columnIndex) -> (tile, rowIndex, columnIndex)) row) [0..]
                               $ fmap (\row -> zip row [0..])
                                 b

-- | Rotate given board clockwise by 90 degrees
rotateBoard :: Board -> Board
rotateBoard (Board b) = Board $ reverse <$> transpose b

-- | A board of empty tiles
emptyBoard :: Board
emptyBoard = Board $ replicate 4 $ replicate 4 Empty

-- | Default starting game state without 2 initial tiles
initialGameState :: StdGen -> GameState
initialGameState g = GameState { board = emptyBoard
                               , score = 0
                               , status = InProgress
                               , gen = g
                               }
