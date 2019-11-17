module Gloss
  ( module Re
  , GlossEvent
  , gloss
  ) where

import Game

import System.Exit
import Data.IORef
import Graphics.Gloss.Interface.IO.Game as Re
import Control.Monad.Reader

type GlossGame = Game GlossEvent Picture (ReaderT GlossParams IO) ()
type GlossEvent  = Either Event Float
type GlossParams = (Display, Color, Int)


-- (>\\)
--   :: Functor m
--   => (Picture -> Game Void Void m GlossEvent)
--   -> Game GlossEvent Picture m r
--   -> Game Void Void m r

gloss
  :: GlossParams
  -> Picture -> Game Void Void IO GlossEvent
gloss = undefined

-- toGloss :: GlossParams
--         -> Game GlossEvent Picture IO ()
--         -> Game Void Void IO ()
-- toGloss (disp, col, fps) game = liftIO $ do
--   ref <- newIORef game
--   playIO disp col fps ref getPic handleEvent handleTime
--     where
--       getPic ref = inspect orExit
--       handleEvent ref = _
--       handleTime ref = _
