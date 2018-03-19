{-# LANGUAGE FlexibleContexts, DataKinds, TypeFamilies, TypeOperators #-}
module Android.Rendering where

import Java

-- Canvas

type Render a = Java Canvas a

foreign import java unsafe
  drawRect :: Float -> Float -> Float -> Float -> Paint -> Render ()

foreign import java unsafe
  drawRoundRect :: Float -> Float -> Float -> Float
                -> Float -> Float -> Paint -> Render ()

foreign import java unsafe drawText :: String -> Float -> Float -> Paint -> Render ()

foreign import java unsafe getHeight :: Render Int

foreign import java unsafe getWidth :: Render Int

foreign import java unsafe translate :: Float -> Float -> Render ()

foreign import java unsafe scale :: Float -> Float -> Render ()

foreign import java unsafe rotate :: Float -> Render ()

foreign import java unsafe save :: Render Int

foreign import java unsafe restore :: Render ()

-- Canvas

-- Paint

data Paint = Paint @android.graphics.Paint
  deriving Class

foreign import java unsafe "@new" newPaint :: IO Paint

data Style = Style @android.graphics.Paint$Style
  deriving Class

foreign import java unsafe "@static @field android.graphics.Paint$Style.FILL_AND_STROKE"
  fILL_AND_STROKE :: Style

data Align = Align @android.graphics.Paint$Align
  deriving Class

foreign import java unsafe "@static @field android.graphics.Paint$Align.CENTER"
  cENTER :: Align

foreign import java unsafe setColor :: Color -> Java Paint ()

foreign import java unsafe setStyle :: Style -> Java Paint ()

foreign import java unsafe setTextAlign :: Align -> Java Paint ()

-- Paint

-- Color

type Color = Int

foreign import java unsafe "@static android.graphics.Color.argb"
  makeColor :: Int -> Int -> Int -> Int -> Color

makeColorI :: Int -> Int -> Int -> Int -> Color
makeColorI r g b a = makeColor a r g b

black, white :: Color
black = makeColorI 0   0   0   255
white = makeColorI 255 255 255 255

-- Paint.Align


-- Paint.Align

-- MotionEvent

data MotionEvent = MotionEvent @android.view.MotionEvent
  deriving Class

-- MotionEvent

-- Swipe

data Swipe = Swipe @com.github.pwittchen.swipe.library.rx2.Swipe
  deriving Class

foreign import java unsafe setListener :: SwipeListener -> Java Swipe ()

data SwipeListener = SwipeListener @com.github.pwittchen.swipe.library.rx2.SwipeListener
  deriving Class

type SwipeHandler = MotionEvent -> Java SwipeListener ()
type BooleanSwipeHandler = MotionEvent -> Java SwipeListener Bool

defaultSwipeHandler :: SwipeHandler
defaultSwipeHandler _ = return ()

foreign import java unsafe
  "@wrapper onSwipingLeft,onSwipedLeft,onSwipingRight,onSwipedRight,onSwipingUp,onSwipedUp,onSwipingDown,onSwipedDown" makeSwipeListener
  :: SwipeHandler -> BooleanSwipeHandler -> SwipeHandler -> BooleanSwipeHandler
  -> SwipeHandler -> BooleanSwipeHandler -> SwipeHandler -> BooleanSwipeHandler
  -> SwipeListener

-- Swipe

data Canvas = Canvas @android.graphics.Canvas
  deriving Class

-- View

data View = View @android.view.View
  deriving Class

foreign import java unsafe "@wrapper @abstract onDraw"
  makeView :: (Canvas -> Java View ()) -> View

foreign import java unsafe invalidate :: Java View ()

data ViewGroup = ViewGroup @android.view.ViewGroup
  deriving Class

type instance Inherits ViewGroup = '[View]

foreign import java unsafe addView :: View -> Java ViewGroup ()

---

data SwipeEvent
 = SwipeLeft
 | SwipeRight
 | SwipeUp
 | SwipeDown

---

data Activity = Context @android.app.Activity
  deriving Class

foreign import java unsafe findViewById :: (a <: View) => Int -> Java Activity a

---

data Handler = Handler @android.os.Handler
  deriving Class

foreign import java unsafe "@new" newHandler  :: Java a Handler
foreign import java unsafe postDelayed :: Runnable -> Int64 -> Java Handler Bool

data Runnable = Runnable @java.lang.Runnable
  deriving Class

foreign import java unsafe "@wrapper run" makeRunnable :: Java Runnable () -> Runnable

-- Utilities

intervalTask :: Int64 -> IO () -> IO ()
intervalTask millis periodicAction = java $ do
  handler <- newHandler

  let runnable = makeRunnable $ do
        io $ periodicAction
        handler <.> postDelayed runnable millis
        return ()

  handler <.> postDelayed runnable millis
  return ()

saveAndRestore :: Render () -> Render ()
saveAndRestore render = save >> render >> restore

clearRect :: Int -> Int -> Int -> Int -> Paint -> Render ()
clearRect left top width height =
  drawRect 0 0 (fromIntegral width) (fromIntegral height)

fillRect :: Float -> Float -> Float -> Float -> Paint -> Render ()
fillRect x y width height paint = drawRect x y (x + width) (y + height) paint

fillRoundRect :: Float -> Float -> Float -> Float
              -> Float -> Float -> Paint -> Render ()
fillRoundRect x y width height rx ry paint =
  drawRoundRect x y (x + width) (y + height) rx ry paint

fillText :: String -> Float -> Float -> Paint -> Render ()
fillText = drawText
