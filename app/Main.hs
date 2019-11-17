{-# LANGUAGE LambdaCase #-}

module Main where

import Game
import Gloss

type V2 = (Int, Int)
data Movement = U | D | L | R
data GameInput = GameMove Movement | GameExit

type VisibleState = V2
type HiddenState = Int
type GameState = (HiddenState, VisibleState)

step :: Movement -> GameState -> GameState
step U (n, (x,y)) = (n + 1, (x, y-1))
step D (n, (x,y)) = (n + 1, (x, y+1))
step L (n, (x,y)) = (n + 1, (x-1, y))
step R (n, (x,y)) = (n + 1, (x+1, y))

state0 :: GameState
state0 = (0, (0,0))

game :: Monad m => Game GameInput VisibleState m ()
game = go state0
  where
    go (h,s) = request s >>= \case
      GameMove m -> go (step m (h,s))
      GameExit   -> pure ()

asGloss :: Game GameInput VisibleState IO () -> Game GlossEvent Picture IO ()
asGloss = undefined

main :: IO ()
main = runGame $ undefined -- >\\ asGloss game
  where
    params = undefined
