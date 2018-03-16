module Types ( Tile(..)
             , Row
             , Column
             , Board(..)
             , GameState(..)
             , GameStatus(..)
             , Direction(..)
             , GameInput
             ) where

import System.Random (StdGen)
import FRP.Yampa (Event)

data Tile = Number Int | Empty deriving (Eq, Show)

type Row = Int
type Column = Int

newtype Board = Board [[Tile]] deriving (Eq, Show)

data GameStatus = InProgress
                | GameOver
                deriving (Eq, Show)

data GameState = GameState { board :: Board
                           , score :: Int
                           , status :: GameStatus
                           , gen :: StdGen
                           } deriving (Show)

data Direction = Up | Down | Left | Right | None deriving (Eq, Show)

type GameInput = Event Direction
