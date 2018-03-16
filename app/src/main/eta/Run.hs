module Run where

import System.Random (newStdGen, StdGen)
import FRP.Yampa (Event(..), SF, arr, tag, (>>>))
import Android.Rendering
import Picture
import Android.FRP
import Android.Main
import Java
import Data.IORef

import Types
import Game
import Rendering

-- | Our game uses up, down, left and right arrows to make the moves, so
-- the first thing we want to do is to parse the Gloss Event into something
-- we are happy to work with (Direction data type)
parseInput :: SF (Event SwipeEvent) GameInput
parseInput = arr f
  where f event@(Event e) =
          case e of
            SwipeUp    -> event `tag` Types.Up
            SwipeDown  -> event `tag` Types.Down
            SwipeLeft  -> event `tag` Types.Left
            SwipeRight -> event `tag` Types.Right
        f event = event `tag` None

-- | After parsing the game input and reacting to it we need to draw the
-- current game state which might have been updated
drawGame :: SF GameState Picture
drawGame = arr drawBoard

-- | Our main signal function which is responsible for handling the whole
-- game process, starting from parsing the input, moving to the game logic
-- based on that input and finally drawing the resulting game state to
-- Gloss' Picture
mainSF :: StdGen -> SF (Event SwipeEvent) Picture
mainSF g = parseInput >>> wholeGame g >>> drawGame

foreign export java "@static eta.game.Run.run"
  run :: Activity -> View -> Swipe -> IO ()
run activity view swipe = do
    g <- newStdGen
    playYampa -- ("2048 game", white, 410, 500)
              view
              swipe
              (mainSF g)

foreign export java "@static eta.game.Run.canvasOnDraw"
  canvasOnDraw :: Canvas -> IO ()
canvasOnDraw canvas = do
  picture <- readIORef worldPicRef
  javaWith canvas $ renderAction worldPaint (drawPicture worldPaint picture)
