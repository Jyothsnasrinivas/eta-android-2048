-- Inspired from Graphics.Gloss.Data.Picture

module Picture where

data Picture
  = Blank
  | Rectangle Float Float
  | RoundRectangle Float Float Float Float
  | Text String
  | Color Int Picture
  | Translate Float Float Picture
  | Rotate Float Picture
  | Scale Float Float Picture
  | Pictures [Picture]
  deriving Show

instance Monoid Picture where
  mempty      = blank
  mappend a b = Pictures [a, b]
  mconcat     = Pictures

blank :: Picture
blank = Blank

rectangleSolid :: Float -> Float -> Picture
rectangleSolid = Rectangle

roundedRect :: Int -> Float -> Float -> Float -> Picture
roundedRect _ w h r = RoundRectangle w h r r

text :: String -> Picture
text = Text

color :: Int -> Picture -> Picture
color = Color

translate :: Float -> Float -> Picture -> Picture
translate = Translate

rotate :: Float -> Picture -> Picture
rotate = Rotate

scale :: Float -> Float -> Picture -> Picture
scale = Scale

pictures :: [Picture] -> Picture
pictures = Pictures

isText :: Picture -> Bool
isText (Text _) = True
isText _        = False
