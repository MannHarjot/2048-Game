{-
Q5 Explanation:
The GUI is built using the Brillo framework. The drawGame function is responsible
for rendering the background, grid, tiles, and overlays like the game-over
message. Keyboard input is handled by handleInput, which converts key presses
into game messages using the update function. The GUI is kept separate from
the game logic and only interacts with the game through messages.
-}

module Main (main) where

import A2.Board (Board (..), SlideDirection (..))
import A2.Game (Game (..), Message (..), Player (..), initGame, update)
import A2.Grid4x4 (at, indices)
import A2.Quadruple (Index (..))
import Brillo
  ( Color,
    Display (InWindow),
    Picture,
    black,
    blank,
    color,
    makeColorI,
    pictures,
    rectangleSolid,
    scale,
    text,
    translate,
    white,
  )
import Brillo.Interface.IO.Game
  ( Event (EventKey),
    Key (Char, SpecialKey),
    KeyState (Down),
    SpecialKey (KeyDown, KeyLeft, KeyRight, KeyUp),
    playIO,
  )
import System.Exit (exitSuccess)
import System.Random (mkStdGen)

windowWidth, windowHeight :: Int
windowWidth = 600
windowHeight = 800

bgColour :: Color
bgColour = makeColorI 250 248 239 255

main :: IO ()
main =
  playIO
    (InWindow "2048" (windowWidth, windowHeight) (100, 100))
    bgColour -- Background Colour
    10 -- FPS (Keep low, else bot goes too fast)
    initialGame -- Initial Game
    drawGame -- Render
    handleInput -- Input Handler
    stepGame -- Time Step (Handles bot auto-step)

initialGame :: Game
initialGame = initGame (mkStdGen 42)

-- While testing, using a fixed seed for the random number generator (StdGen) is
-- useful, e.g., `mkStdGen 42`. However, you should also test using the global
-- random number generator, `getStdGen`.

-- -----------------------------------------------------------------------------
-- Logic
-- -----------------------------------------------------------------------------

-- | Bot operates on each frame render.
stepGame :: Float -> Game -> IO Game
stepGame _ g@(Playing (Bot _) _) = return $ update BotStep g
stepGame _ g = return g

-- | Updates the game state given input events.
handleInput :: Event -> Game -> IO Game
handleInput (EventKey (Char 'q') Down _ _) _ =
  exitSuccess

handleInput (EventKey (Char 'r') Down _ _) g =
  pure $ update Restart g

handleInput (EventKey (Char 'b') Down _ _) g =
  pure $ update ToggleBot g

handleInput (EventKey (Char 'w') Down _ _) g =
  pure $ update (Slide SUp) g

handleInput (EventKey (Char 'a') Down _ _) g =
  pure $ update (Slide SLeft) g

handleInput (EventKey (Char 's') Down _ _) g =
  pure $ update (Slide SDown) g

handleInput (EventKey (Char 'd') Down _ _) g =
  pure $ update (Slide SRight) g

handleInput (EventKey (SpecialKey KeyUp) Down _ _) g =
  pure $ update (Slide SUp) g

handleInput (EventKey (SpecialKey KeyLeft) Down _ _) g =
  pure $ update (Slide SLeft) g

handleInput (EventKey (SpecialKey KeyDown) Down _ _) g =
  pure $ update (Slide SDown) g

handleInput (EventKey (SpecialKey KeyRight) Down _ _) g =
  pure $ update (Slide SRight) g

handleInput _ g =
  pure g


-- -----------------------------------------------------------------------------
-- Rendering
-- -----------------------------------------------------------------------------

drawBackground :: Picture
drawBackground =
  color bgColour $
    rectangleSolid
      (fromIntegral windowWidth)
      (fromIntegral windowHeight)

drawGrid :: Game -> Picture
drawGrid (Playing _ b) = drawBoard b
drawGrid (GameOver b)  = drawBoard b
drawGrid Quitted       = blank

drawBoard :: Board -> Picture
drawBoard b =
  pictures
    [ drawTile ij (at (grid b) ij)
    | ij <- indices
    ]

tileSize :: Float
tileSize = 120

tilePadding :: Float
tilePadding = 10

gridOriginX, gridOriginY :: Float
gridOriginX = -240
gridOriginY = 200

drawTile :: (Index, Index) -> Word -> Picture
drawTile (x, y) v =
  translate px py $
    pictures
      [ color (tileColour v) (rectangleSolid tileSize tileSize)
      , if v == 0
          then blank
          else translate (-30) (-20) $
                 scale 0.2 0.2 $
                   color black $
                     text (show v)
      ]
  where
    ix = case x of I0 -> 0; I1 -> 1; I2 -> 2; I3 -> 3
    iy = case y of I0 -> 0; I1 -> 1; I2 -> 2; I3 -> 3

    px = gridOriginX + fromIntegral ix * (tileSize + tilePadding)
    py = gridOriginY  - fromIntegral iy * (tileSize + tilePadding)

drawOverlay :: Game -> Picture
drawOverlay (GameOver _) =
  translate (-200) 0 $
    scale 0.4 0.4 $
      color black $
        text "Game Over!"
drawOverlay _ = blank

drawInstructions :: Picture
drawInstructions =
  translate (-260) (-320) $
    scale 0.15 0.15 $
      color black $
        text "Arrows/WASD to move  |  (B) Toggle Bot  |  (R) Restart  |  (Q) Quit"

drawPlayer :: Game -> Picture
drawPlayer (Playing Human _) =
  translate (-260) 260 $
    scale 0.2 0.2 $
      color black $
        text "Human"

drawPlayer (Playing (Bot _) _) =
  translate (-260) 260 $
    scale 0.2 0.2 $
      color black $
        text "Bot"

drawPlayer _ = blank

-- | Renders the game given its current state.
drawGame :: Game -> IO Picture
drawGame g =
  pure $
    pictures
      [ drawBackground
      , drawGrid g
      , drawOverlay g
      , drawInstructions
      , drawPlayer g
      ]


-- | Adapted from <https://github.com/gabrielecirulli/2048/blob/master/style/main.css>
tileColour :: Word -> Color
tileColour 0 = makeColorI 205 193 180 255
tileColour 2 = makeColorI 238 228 218 255 -- #eee4da
tileColour 4 = makeColorI 237 224 200 255 -- #ede0c8
tileColour 8 = makeColorI 242 177 121 255 -- #f2b179
tileColour 16 = makeColorI 245 149 99 255 -- #f59563
tileColour 32 = makeColorI 246 124 95 255 -- #f67c5f
tileColour 64 = makeColorI 246 94 59 255 -- #f65e3b
tileColour 128 = makeColorI 237 207 114 255 -- #edcf72
tileColour 256 = makeColorI 237 204 97 255 -- #edcc61
tileColour 512 = makeColorI 237 200 80 255 -- #edc850
tileColour 1024 = makeColorI 237 197 63 255 -- #edc53f
tileColour 2048 = makeColorI 237 194 46 255 -- #edc22e
tileColour _ = tileColour 2048