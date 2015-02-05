module TicTacToe.AI where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad

import qualified Data.Foldable as F

import Data.List (group, sort, maximumBy, minimumBy)
import Data.Maybe (catMaybes, isNothing)
import Data.Ord (comparing)
import Data.Monoid

import TicTacToe.Logic

aiLevel :: Num a => a
aiLevel = 3

newtype E = E { getE :: [Int] } deriving (Eq, Show)

instance Ord E where
  E xs `compare` E ys = mconcat $ align compare 0 xs ys

instance Monoid E where
  mempty = E []
  E xs `mappend` E ys = E $ align (+) 0 xs ys

align :: (a -> a -> b) -> a -> [a] -> [a] -> [b]
align f d xs [] = zipWith f xs (repeat d)
align f d [] ys = zipWith f (repeat d) ys
align f d (x:xs) (y:ys) = f x y : align f d xs ys

potentLine :: Bool -> Int -> E
potentLine self n = E $ replicate (gridSize - n) 0 ++ [if self then 1 else -1]

estimateLine :: Mark -> [Maybe Mark] -> E
estimateLine player = estimateLine' . group . sort . catMaybes
  where
    estimateLine' [ms@(m:_)] = potentLine (m == player) (length ms)
    estimateLine' _ = mempty

estimate :: Mark -> Board -> E
estimate player = F.foldMap (estimateLine player . map snd) . boardLines

data Tree m a = Tree a [(m, Tree m a)] deriving (Show)

instance Functor (Tree m) where
  fmap f (Tree x ts) = Tree (f x) (map (second $ fmap f) ts)

data LeafTree m a = Leaf a | Node [(m, LeafTree m a)] deriving (Show)

second :: (b -> c) -> (a, b) -> (a, c)
second f (x, y) = (x, f y)

instance Functor (LeafTree m) where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node ts) = Node $ map (second $ fmap f) ts

gameTree :: Mark -> Board -> Tree Cell Board
gameTree player board = Tree board $ map moveTree (vacantCells board)
  where
    moveTree cell = (cell, gameTree player' board')
      where
        player' = switchPlayer player
        board' = move cell player board

cutTree :: Int -> Tree m a -> Tree m a
cutTree n (Tree x ts)
  | n < 1     = Tree x []
  | otherwise = Tree x (map (second $ cutTree (n - 1)) ts)

filterTree :: (a -> Bool) -> Tree m a -> Tree m a
filterTree p (Tree x ts)
  | p x       = Tree x (map (second $ filterTree p) ts)
  | otherwise = Tree x []

toLeafTree :: Tree m a -> LeafTree m a
toLeafTree (Tree x []) = Leaf x
toLeafTree (Tree _ ts) = Node $ map (second toLeafTree) ts

minimax :: Ord a => [(m, LeafTree m a)] -> (m, a)
minimax = maximumBy (comparing snd) . map (second g)
  where
    g (Leaf x) = x
    g (Node ts) = snd (minimax' ts)

    h (Leaf x) = x
    h (Node ts) = snd (minimax ts)

    minimax' = minimumBy (comparing snd) . map (second h)

ai :: Mark -> Board -> Maybe Cell
ai player board =
  case fmap (estimate player) . toLeafTree . filterTree (isNothing . winner) . cutTree aiLevel $ gameTree player board of
    Node xs@(_:_) -> Just (fst (minimax xs))
    _ -> Nothing

aiMove :: GameState -> GameState
aiMove g = 
  case ai (gamePlayer g) (gameBoard g) of
    Just cell -> gameMove cell g
    Nothing -> g

runAI :: TVar GameState -> Mark -> IO ()
runAI tg m = do
  _ <- forkIO aiLoop
  return ()
  where
    aiLoop = do
      g <- atomically $ do
        g <- readTVar tg
        when (gamePlayer g /= m) retry
        return g
      when (not (gameOver g)) $ do
        atomically $ writeTVar tg (aiMove g)
        runAI tg m

