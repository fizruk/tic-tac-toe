module Main where

import Common

import Graphics.Gloss.Interface.IO.Game

import Data.Binary
import Data.Binary.Get (runGet)
import qualified Data.ByteString.Lazy as BSL

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (when)

import qualified Network.Simple.TCP as S
import Network.Socket (socketToHandle)

import GHC.Generics

import System.IO

windowWidth :: Num a => a
windowWidth = boardSize

windowHeight :: Num a => a
windowHeight = boardSize

updateGameState :: S.Socket -> TVar GameState -> IO ()
updateGameState sock tg = do
  putStrLn "ololo"
  interactWithSocket sock $ \g -> do
    putStrLn "got something"
    atomically $ writeTVar tg g

playerMove' :: S.Socket -> Cell -> TicTacToe -> IO TicTacToe
playerMove' sock cell t = do
  g <- atomically $ readTVar (tGameState t)
  when (gamePlayer g `notElem` tAI t && not (gameOver g) && isVacant cell (gameBoard g)) $ do
    let g' = gameMove cell g
    sendBinary sock (CmdMove cell)
    atomically $ writeTVar (tGameState t) g'
  return t { tHoverCell = Nothing }


handleTicTacToe' :: S.Socket -> Event -> TicTacToe -> IO TicTacToe
handleTicTacToe' sock (EventKey (MouseButton LeftButton) Down _ point) = playerMove' sock (toCell point)
handleTicTacToe' _ (EventMotion point) = updateHoverCell (toCell point)
handleTicTacToe' _ _ = return

main :: IO ()
main = S.withSocketsDo $ do
  putStrLn "Started client"
  S.connect "127.0.0.1" "8000" $ \(sock, addr) -> do
    t <- initialTicTacToe
    _ <- forkIO (updateGameState sock (tGameState t))
    playIO display bgColor fps t {tAI = [O]} drawTicTacToe (handleTicTacToe' sock) updateTicTacToe
  where
    display = InWindow "Tic Tac Toe" (windowWidth, windowHeight) (200, 200)
    bgColor = white
    fps = 30

