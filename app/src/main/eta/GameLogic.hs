module GameLogic ( placeRandomTile
                 , slideBoard
                 , isGameOver
                 ) where

import Prelude ((==), (/=), (>), (*), (<), fromIntegral, floor, fst, snd, ($), Float, Int, Bool, Eq, error)
import Data.List (and, length, sum, filter, concat, (++), replicate, take, (!!))
import Data.Maybe (Maybe(..), isNothing, fromMaybe)
import Data.Functor (fmap)
import Control.Applicative ((<$>))
import Control.Category ((.), id)

import Types
import GameModel

-- | The probably that a new tile is a 2. What remains here is 0.1
-- probability to get a 4 as a new tile
tile2Probability :: Float
tile2Probability = 0.9

-- | Based on the random value provided we create a new tile which might be
-- either 2 or 4
newTile :: Float -> Tile
newTile x = Number $ if x < tile2Probability then 2 else 4

-- | Find all empty tiles on the board and return them as a list of their
-- coordinates
emptyTiles :: Board -> [(Row, Column)]
emptyTiles b = fmap (\(_, r, c) -> (r, c))
             $ filter (\(tile, _, _) -> tile == Empty)
             $ tilesWithCoordinates b

-- | Return a random (based on input Float) tile (its coordinates on
-- a board) if it is possible. There might be a situation when the game
-- board is full of tiles but the game is not yet over
newTileIndex :: Float -> Board -> Maybe (Row, Column)
newTileIndex x b =
    let emptyTileIndices = emptyTiles b
    in case emptyTileIndices of
         [] -> Nothing
         _ -> Just $ emptyTileIndices !! floor (fromIntegral (length emptyTileIndices) * x)

-- | Place random tile into a random position on a board. Randomness
-- depends on the input Floats
placeRandomTile :: Float -> Float -> GameState -> GameState
placeRandomTile float1 float2 gameState =
    let tileIndex = newTileIndex float1 (board gameState)
    in if isNothing tileIndex
         then gameState
         else gameState { board = setTile (fromMaybe (0, 0) tileIndex) (board gameState) $ newTile float2 }

-- | Takes a list of values and 'slides' them to the left, joining in lists
-- of pairs of adjacent identical values
--
-- groupedByTwo ["a","b","b","b","c","d","c","c","c","c"] = 
--              [["a"],["b","b"],["b"],["c"],["d"],["c","c"],["c","c"]]
--
-- groupedByTwo [1, 1, 1] = [[1, 1], [1]]
groupedByTwo :: Eq a => [a] -> [[a]]
groupedByTwo l = case l of
                   [x] -> [[x]]
                   [x, y] -> if x == y then [[x, y]] else [[x], [y]]
                   (x:y:xs) -> if x == y
                                 then [x, y] : groupedByTwo xs
                                 else [x] : groupedByTwo (y : xs)
                   _ -> []

-- | Slides list of tiles to the left, merging tiles where necessary, and
-- returning a full list of four tiles, and the number of points gained
slideRow :: [Tile] -> ([Tile], Int)
slideRow row = let grouped = groupedByTwo $ filter (/= Empty) row
               in ( take 4
                    $ fmap (intToTile . sum . fmap tileToInt) grouped ++ replicate 4 Empty
                  , sum . fmap tileToInt $ concat $ filter (\x -> length x > 1) grouped
                  )

-- | Slide all of the rows (or columns) of the board in a certain direction
slideBoard :: Direction -> Board -> (Board, Int)
slideBoard direction b =
    let rotatedBoard = (case direction of
                          Down -> rotateBoard
                          Right -> rotateBoard . rotateBoard
                          Up -> rotateBoard . rotateBoard . rotateBoard
                          Left -> id
                          _ -> error "unexpected direction") b

        rowsWithScores = slideRow <$> (\(Board x) -> x) rotatedBoard

        slidRotatedBoard = Board $ fmap fst rowsWithScores
        scoreGained = sum $ fmap snd rowsWithScores

        slidBoard = (case direction of
                       Up -> rotateBoard
                       Right -> rotateBoard . rotateBoard
                       Down -> rotateBoard . rotateBoard . rotateBoard
                       Left -> id
                       _ -> error "unexpected direction") slidRotatedBoard

    in (slidBoard, scoreGained)

-- | Check if the game is over. Conditions are simple: it is not an empty
-- board and 'sliding' the board into any direction doesn't result in
-- different board. Although it might be easier to check if the score of
-- 'sliding' is 0 instead of comparing the boards
isGameOver :: GameState -> Bool
isGameOver gameState =
    let b = board gameState
        slidUp = fst $ slideBoard Up b
        slidDown = fst $ slideBoard Down b
        slidLeft = fst $ slideBoard Left b
        slidRight = fst $ slideBoard Right b
    in and [ b /= emptyBoard
           , slidUp == slidDown
           , slidDown == slidLeft
           , slidLeft == slidRight
           , slidRight == b
           ]
