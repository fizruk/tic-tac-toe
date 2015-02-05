{-# LANGUAGE DeriveGeneric #-}
module TicTacToe.Network where

import TicTacToe.Logic

import Data.Binary
import Data.Binary.Get (runGet)
import qualified Data.ByteString.Lazy as BSL

import Control.Applicative

import qualified Network.Simple.TCP as S
import Network.Socket (socketToHandle)

import GHC.Generics

import System.IO

data Command = CmdMove Cell deriving (Generic)

sendBinary :: Binary a => S.Socket -> a -> IO ()
sendBinary sock = S.send sock . BSL.toStrict . encode

instance Binary Command
instance Binary Mark
instance Binary GameState

interactWithSocket :: Binary a => S.Socket -> (a -> IO ()) -> IO ()
interactWithSocket sock f = do
  h <- socketToHandle sock ReadMode
  bs <- BSL.hGetContents h
  mapM_ f (runGet (many get) bs)

