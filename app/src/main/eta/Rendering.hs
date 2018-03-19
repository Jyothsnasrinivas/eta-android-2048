module Rendering (drawBoard) where

import Android.Rendering (Color, makeColorI, black)
import Picture

import GameModel
import Types

rowHeight :: Float
rowHeight = 240

tilePrecision :: Int
tilePrecision = 24

tileS :: Float
tileS = 216

tileRoundness :: Float
tileRoundness = 20

textScale :: Float
textScale = 8

tileBackColor :: Color
tileBackColor = makeColorI 205 192 180 255

drawTileBack :: Float -> Picture
drawTileBack x = color tileBackColor (translate x 0 (roundedRect tilePrecision tileS tileS tileRoundness))

-- Takes x-offset and tile and draws the tile itself
drawTile :: Float -> Tile -> Picture
drawTile x tile =
    let background = [color (tileColor tile) $ roundedRect tilePrecision tileS tileS tileRoundness]
        number = if tileToInt tile > 0
                   then [color black
                         $ translate 0 (-22)
                         $ scale scaleFactor scaleFactor
                         $ text $ show $ tileToInt tile]
                   else []
        scaleFactor = textScale
        curScale = 1
    in pictures [ drawTileBack x
                , translate x 0 $ scale curScale curScale $ pictures $ background ++ number
                ]

drawRow :: [Tile] -> Picture
drawRow tile =
    let [i, j, k, l] = tile
    in translate (-300) 0 (pictures [ drawTile 0 i
                                    , drawTile rowHeight j
                                    , drawTile (rowHeight * 2) k
                                    , drawTile (rowHeight * 3) l
                                    ])

gameOverMessage :: Picture
gameOverMessage = pictures [ translate (-150) (-525) $
                             color translucentWhite $ rectangleSolid 500 500
                           , translate (-30) 10 $
                             scale textScale textScale $ color black $ text "Game Over" ]
  where translucentWhite = makeColorI 255 255 255 150

-- | Draw current board representation depending on the status of the game.
-- All tiles will be drawn at all tiles and game over message is drawn onto
-- the game board when game status is GameOver
drawBoard :: GameState -> Picture
drawBoard gameState =
    let (Board b) = board gameState
        [r1, r2, r3, r4] = b
    in translate (-60) 0 $
       pictures
     $ [ drawRow r1
       , translate 0 (-rowHeight) (drawRow r2)
       , translate 0 (-rowHeight * 2) (drawRow r3)
       , translate 0 (-rowHeight * 3) (drawRow r4)
       , translate (-20) (-50) $
         scale 10 10 $
         color black $ text $ "Score: " ++ show (score gameState)
       ] ++ gameOverPicture
  where gameOverPicture = [gameOverMessage | status gameState == GameOver]

-- | Tile colors up to tile with value 2048 taken directly from the
-- original game. The rest of the numbers should be assigned some good
-- values (well we can easily reach tile 4096 and some AI reach 32768)
tileColor :: Tile -> Color
tileColor tile = case tile of
                   Number 2     -> makeColorI 238 228 218 255
                   Number 4     -> makeColorI 237 224 200 255
                   Number 8     -> makeColorI 242 177 121 255
                   Number 16    -> makeColorI 245 149 99 255
                   Number 32    -> makeColorI 246 124 95 255
                   Number 64    -> makeColorI 246 94 59 255
                   Number 128   -> makeColorI 237 207 114 255
                   Number 256   -> makeColorI 237 204 97 255
                   Number 512   -> makeColorI 237 200 80 255
                   Number 1024  -> makeColorI 237 197 63 255
                   Number 2048  -> makeColorI 237 194 46 255
                   _            -> makeColorI 238 228 218 90
