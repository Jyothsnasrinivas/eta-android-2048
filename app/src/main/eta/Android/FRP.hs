module Android.FRP where

import Control.Monad (when)
import Data.IORef (newIORef, writeIORef, readIORef, modifyIORef')
import Picture (Picture, blank)
import Android.Rendering (Color, Swipe, View, SwipeEvent, Paint, newPaint)
import Android.Main (playAndroid)
import FRP.Yampa (Event(..), SF, reactInit, react)
import Data.IORef
import System.IO.Unsafe

playYampa :: View -> Swipe -> SF (Event SwipeEvent) Picture -> IO ()
playYampa view swipe mainSF = do
  updateRef <- newIORef 0

  handle <- reactInit
    (return NoEvent)
    (\_ updated pic -> do
      when updated $ do
        worldPicRef `writeIORef` pic
        modifyIORef' updateRef (+ 1)
      return False)
    mainSF

  writeIORef worldSR 0

  playAndroid view updateRef swipe worldPaint worldSR
              (\e t -> react handle (delta, Just (Event e)) >> return (t + delta))
              (\d t -> let delta' = realToFrac d - t
                        in if delta' > 0
                          then react handle (delta', Just NoEvent) >> return 0.0
                          else return (-delta'))

  where delta = 0.01 / frequency
        frequency = 60

worldSR :: IORef Double
worldSR = unsafePerformIO (newIORef 0.0)

worldPaint :: Paint
worldPaint = unsafePerformIO newPaint

worldPicRef :: IORef Picture
worldPicRef = unsafePerformIO (newIORef blank)
