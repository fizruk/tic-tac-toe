module Main where

import TicTacToe.AI
import TicTacToe.Logic
import TicTacToe.Network

import Control.Concurrent
import Control.Concurrent.STM

import qualified Network.Simple.TCP as S

main :: IO ()
main = S.withSocketsDo $ do
  putStrLn "Starting server"
  S.serve S.HostAny "8000" $ \(sock, addr) -> do
    putStrLn $ "Established a connection at " ++ show addr
    clientChan <- atomically newTChan
    let send x = do
          putStrLn "sending something"
          sendBinary sock x
        recv = do
          x <- atomically $ readTChan clientChan
          putStrLn "received something"
          return x
    _ <- forkIO $ interactWithSocket sock (atomically . writeTChan clientChan)
    gameLoop send recv initialGameState [O]

gameLoop :: (GameState -> IO ()) -> IO Command -> GameState -> [Mark] -> IO ()
gameLoop send recv g aiMarks = go g
  where
    go g = do
      print $ gamePlayer g
      g' <- if gamePlayer g `elem` aiMarks
              then do
                let g' = aiMove g
                send g'
                return g'
              else do
                CmdMove cell <- recv
                print cell
                return $ if isVacant cell (gameBoard g)
                           then gameMove cell g
                           else g
      go g'

