module TicTacToe.Interface where

import Control.Concurrent.STM
import Control.Monad
import Data.Monoid
import Graphics.Gloss.Interface.Pure.Game

import TicTacToe.Logic

boardSize :: Num a => a
boardSize = 400

cellSize :: Floating a => a
cellSize = boardSize / gridSize

swap :: (a, b) -> (b, a)
swap (x, y)  = (y, x)

data TicTacToe = TicTacToe
  { tGameState  :: TVar GameState
  , tHoverCell  :: Maybe Cell
  , tAI         :: [Mark]
  }

initialTicTacToe :: IO TicTacToe
initialTicTacToe = do
  g <- atomically $ newTVar initialGameState
  return $ TicTacToe g Nothing []

drawTicTacToe :: TicTacToe -> IO Picture
drawTicTacToe t = do
  g <- atomically $ readTVar (tGameState t)
  return . scale cellSize cellSize . translate (-gridSize / 2) (-gridSize / 2) $ drawGameState g <> drawHoverCell (gamePlayer g) hcell
  where
    hcell   = tHoverCell t

translateCell :: Cell -> Picture -> Picture
translateCell (x, y) = translate (0.5 + fromIntegral x) (0.5 + fromIntegral y)

drawGameState :: GameState -> Picture
drawGameState g = drawWinner (gameWinner g) <> drawBoard (gameBoard g) <> drawGrid

drawCell :: Cell -> Mark -> Picture
drawCell cell = translateCell cell . drawMark

drawHoverCell :: Mark -> Maybe Cell -> Picture
drawHoverCell _ Nothing = blank
drawHoverCell player (Just cell) = color (dark white) $ drawCell cell player

drawBoard :: Board -> Picture
drawBoard = pictures . map (uncurry drawCell) . boardMarks

drawWinnerCell :: Cell -> Picture
drawWinnerCell cell = color (light red) . translateCell cell . scale 0.5 0.5 $ polygon [(-1, -1), (1, -1), (1, 1), (-1, 1)]

drawWinner :: Maybe (Mark, [Cell]) -> Picture
drawWinner Nothing = blank
drawWinner (Just (_, cs)) = pictures $ map drawWinnerCell cs

drawGrid :: Picture
drawGrid = pictures . map line $ hs ++ vs
  where
    hs = [ [(i, 0), (i, gridSize)] | i <- [1 .. gridSize - 1] ]
    vs = map (map swap) hs

drawMark :: Mark -> Picture
drawMark O = drawO
drawMark X = drawX

drawO :: Picture
drawO = thickCircle 0.33 0.14

drawX :: Picture
drawX = scale 0.1 0.1 $ polygon diag <> polygon diag'
  where
    diag  = [ (-4, 3), (-3, 4), ( 4, -3), ( 3, -4) ]
    diag' = [ ( 4, 3), ( 3, 4), (-4, -3), (-3, -4) ]

playerMove :: Cell -> TicTacToe -> IO TicTacToe
playerMove cell t = do
  g <- atomically $ readTVar (tGameState t)
  when (gamePlayer g `notElem` tAI t && not (gameOver g) && isVacant cell (gameBoard g)) $ do
    let g' = gameMove cell g
    atomically $ writeTVar (tGameState t) g'
  return t { tHoverCell = Nothing }

updateHoverCell :: Cell -> TicTacToe -> IO TicTacToe
updateHoverCell cell t = do
  g <- atomically $ readTVar (tGameState t)
  return $ if (gamePlayer g `notElem` tAI t && not (gameOver g) && isVacant cell (gameBoard g))
             then t { tHoverCell = Just cell }
             else t { tHoverCell = Nothing }

toCell :: Point -> Cell
toCell (x, y) = (floor ((x + boardSize / 2) / cellSize), floor ((y + boardSize / 2) / cellSize))

handleTicTacToe :: Event -> TicTacToe -> IO TicTacToe
handleTicTacToe (EventKey (MouseButton LeftButton) Down _ point) = playerMove (toCell point)
handleTicTacToe (EventMotion point) = updateHoverCell (toCell point)
handleTicTacToe _ = return

updateTicTacToe :: Float -> TicTacToe -> IO TicTacToe
updateTicTacToe _ = return

