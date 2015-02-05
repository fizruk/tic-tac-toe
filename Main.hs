module Main where

import Data.Monoid
import Graphics.Gloss.Interface.IO.Game

import TicTacToe.AI
import TicTacToe.Logic
import TicTacToe.Interface

windowWidth :: Num a => a
windowWidth = boardSize

windowHeight :: Num a => a
windowHeight = boardSize

data ButtonState
  = BtnDefault
  | BtnHover
  | BtnPressed

data Button = Button
  { btnText   :: String
  , btnState  :: ButtonState
  , btnAction :: [Mark]
  }

data Interface
  = IMenu [Button]
  | IGame TicTacToe

initialInterface :: Interface
initialInterface = IMenu
  [ Button "Player vs. Player"  BtnDefault  []
  , Button "Player vs. AI (O)"  BtnDefault  [O]
  , Button "Player vs. AI (X)"  BtnDefault  [X]
  , Button "AI vs. AI"          BtnDefault  [X, O]
  ]

drawInterface :: Interface -> IO Picture
drawInterface (IMenu bs) = return . translate (- windowWidth / 2) (windowHeight / 2) . mconcat $ zipWith drawButton [1..] bs
drawInterface (IGame t) = drawTicTacToe t

buttonHeight :: Num a => a
buttonHeight = 50

buttonPadding :: Num a => a
buttonPadding = 10

drawButton :: Float -> Button -> Picture
drawButton dy btn =
  translate 0 (- buttonHeight * dy) (color c btnBg) <>
  translate (windowWidth / 4) (buttonPadding - buttonHeight * dy) (scale 0.2 0.2 (text t))
  where
    c = btnColor (btnState btn)
    t = btnText btn
    btnBg = polygon [(0, 0), (0, buttonHeight), (windowWidth, buttonHeight), (windowWidth, 0)]

btnColor :: ButtonState -> Color
btnColor BtnDefault = white
btnColor BtnHover   = yellow
btnColor BtnPressed = red

toButtonIndex :: Point -> Int
toButtonIndex (_, y) = floor ((windowHeight / 2 - y) / buttonHeight)

resetButton :: Button -> Button
resetButton btn = btn { btnState = BtnDefault }

hoverButton :: Button -> Button
hoverButton btn =
  case btnState btn of
    BtnPressed -> btn
    _ -> btn { btnState = BtnHover }

pressButton :: Button -> Button
pressButton btn = btn { btnState = BtnPressed }

updateButton :: (Button -> Button) -> Int -> [Button] -> [Button]
updateButton _ _ [] = []
updateButton f n (x:xs)
  | n <= 0    = f x : map resetButton xs
  | otherwise = resetButton x : updateButton f (n - 1) xs

activateButton :: Int -> [Button] -> IO Interface
activateButton n bs =
  case drop n bs of
    (Button _ BtnPressed aiMarks : _) -> do
      t <- initialTicTacToe
      mapM_ (runAI (tGameState t)) aiMarks
      return $ IGame t { tAI = aiMarks }
    _ -> return $ IMenu bs

handleMenu :: Event -> [Button] -> IO Interface
handleMenu (EventMotion point) bs = return $ IMenu (updateButton hoverButton (toButtonIndex point) bs)
handleMenu (EventKey (MouseButton LeftButton) Down _ point) bs = return $ IMenu (updateButton pressButton (toButtonIndex point) bs)
handleMenu (EventKey (MouseButton LeftButton) Up _ point) bs = activateButton (toButtonIndex point) bs
handleMenu _ bs = return $ IMenu bs

handleInterface :: Event -> Interface -> IO Interface
handleInterface e (IMenu bs) = handleMenu e bs
handleInterface e (IGame t) = fmap IGame (handleTicTacToe e t)

updateInterface :: Float -> Interface -> IO Interface
updateInterface dt (IGame t) = fmap IGame (updateTicTacToe dt t)
updateInterface _ i = return i

main :: IO ()
main = do
  playIO display bgColor fps initialInterface drawInterface handleInterface updateInterface
  where
    display = InWindow "Tic Tac Toe" (windowWidth, windowHeight) (200, 200)
    bgColor = white
    fps = 30

