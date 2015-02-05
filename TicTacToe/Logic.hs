{-# LANGUAGE DeriveGeneric #-}
module TicTacToe.Logic where

import GHC.Generics
import Data.List ((\\), tails, find)
import Data.Maybe (isJust)
import Data.Tuple (swap)

import Data.Map (Map)
import qualified Data.Map as Map

type Cell = (Int, Int)

data Mark = O | X deriving (Eq, Ord, Show, Generic)

type Board = Map Cell Mark

data GameState = GameState
  { gameBoard   :: Board
  , gamePlayer  :: Mark
  , gameWinner  :: Maybe (Mark, [Cell])
  }
  deriving (Generic)

initialGameState :: GameState
initialGameState = GameState Map.empty X Nothing

gridSize :: Num a => a
gridSize = 5

winLength :: Num a => a
winLength = 5

gridCells :: [Cell]
gridCells = [ (i, j) | i <- [0 .. gridSize - 1], j <- [0 .. gridSize - 1] ]

vacantCells :: Board -> [Cell]
vacantCells board = gridCells \\ Map.keys board

winningLine :: [(Cell, Maybe Mark)] -> Bool
winningLine ((_, m@(Just _)):xs) = all (== m) $ map snd xs
winningLine _ = False

segments :: [a] -> [[a]]
segments xs = take (gridSize - winLength + 1) $ map (take winLength) (tails xs)

-- FIXME: not all diagonals when winLength < gridSize
boardLines :: Board -> [[(Cell, Maybe Mark)]]
boardLines board = concatMap (segments . map (\cell -> (cell, Map.lookup cell board))) $ hs ++ vs ++ ds
  where
    hs = [ [ (i, j) | j <- [0 .. gridSize - 1] ] | i <- [0 .. gridSize - 1] ]
    vs = map (map swap) hs
    ds = [ [ (i, i) | i <- [0 .. gridSize - 1] ]
         , [ (i, gridSize - i - 1) | i <- [0 .. gridSize - 1] ] ]

winner :: Board -> Maybe (Mark, [Cell])
winner board =
  case find winningLine (boardLines board) of
    Just xs@((_, Just m):_) -> Just (m, map fst xs)
    _ -> Nothing

isVacant :: Cell -> Board -> Bool
isVacant cell board = cell `Map.notMember` board

move :: Cell -> Mark -> Board -> Board
move = Map.insert

switchPlayer :: Mark -> Mark
switchPlayer O = X
switchPlayer X = O

boardMarks :: Board -> [(Cell, Mark)]
boardMarks = Map.assocs

atCell :: Cell -> Board -> Maybe Mark
atCell = Map.lookup

gameMove :: Cell -> GameState -> GameState
gameMove cell g = g { gameBoard = board', gamePlayer = player', gameWinner = winner board' }
  where
    player  = gamePlayer g
    player' = switchPlayer player
    board'  = move cell player (gameBoard g)

gameOver :: GameState -> Bool
gameOver = isJust . gameWinner
