{-# LANGUAGE MagicHash #-}
module Android.Main where

import System.Environment
import Data.IORef
import Data.Proxy
import System.IO.Unsafe
import Java
import Picture (Picture(..), isText)
import Android.Rendering
import Control.Monad

playAndroid :: View -> IORef Int64 -> Swipe
            -> Paint -> IORef world
            -> (SwipeEvent -> world -> IO world)
            -> (Float -> world -> IO world)

            -> IO ()
playAndroid view updateRef swipe worldPaint
  worldSR worldHandleEvent worldAdvance = do

  myUpdateRef <- newIORef =<< readIORef updateRef

  -- Setup swipe event handlers
  let swipeHandler swipeEvent _ = io $ do
        world  <- readIORef worldSR
        world' <- worldHandleEvent swipeEvent world
        writeIORef worldSR world'
        return True

  javaWith swipe . setListener $
    makeSwipeListener
      defaultSwipeHandler (swipeHandler SwipeLeft)
      defaultSwipeHandler (swipeHandler SwipeRight)
      defaultSwipeHandler (swipeHandler SwipeUp)
      defaultSwipeHandler (swipeHandler SwipeDown)

  javaWith worldPaint $ do
    setStyle fILL_AND_STROKE
    setTextAlign cENTER

  -- Setup periodic task to check if world is updated and invalidate the canvas
  intervalTask millis (mainLoop worldSR myUpdateRef view)
  where millis  = round (1000 / 60)
        seconds = fromIntegral millis / 1000

        mainLoop worldSR myUpdateRef view = do
          world   <- readIORef worldSR

          -- Only invalidate the view if we're out of sync
          updateState  <- readIORef myUpdateRef
          updateState' <- readIORef updateRef
          when (updateState /= updateState') $ do
            javaWith view invalidate
            writeIORef myUpdateRef updateState'

          world' <- worldAdvance seconds world
          writeIORef worldSR world'

renderAction :: Paint -> Render () -> Render ()
renderAction paint render = do
  width  <- getWidth
  height <- getHeight
  paint <.> setColor white
  clearRect 0 0 width height paint
  saveAndRestore $ do
    -- Center the coordinate system
    translate (fromIntegral width / 2) (fromIntegral height / 2)
    scale 1 (-1)
    render

drawPicture :: Paint -> Picture -> Render ()
drawPicture paint = go []
  where go [] picture = do
          case picture of
            Blank                    -> return ()
            Rectangle w h            -> fillRect (-w / 2) (h / 2) w h paint
            RoundRectangle w h rw rh -> fillRoundRect (-w / 2) (h / 2) w h rw rh paint
            Text t                   -> fillText t 0 0 paint
            Pictures ps              -> mapM_ (go []) ps
            _                        -> go [Blank] picture
        go ts picture = do
          case picture of
            Color     c   p -> go (Color     c   Blank : ts) p
            Translate x y p -> go (Translate x y Blank : ts) p
            Rotate    deg p -> go (Rotate    deg Blank : ts) p
            Scale     x y p -> go (Scale     x y Blank : ts) p
            p               -> saveAndRestore $ do
              mapM_ applyTrans (if isText p
                                then Scale 1 (-1) Blank : ts
                                else ts)
              go [] p
        applyTrans (Color     c   _) = paint <.> setColor c
        applyTrans (Translate x y _) = translate x y
        applyTrans (Rotate    deg _) = rotate deg
        applyTrans (Scale     x y _) = scale x y
        applyTrans _                 = return ()
