{-# LANGUAGE RankNTypes #-}

module Game
  ( Game (..)
  , request
  , await
  , nest
  , MFunctor (..)
  , MMonad (..)
  , MonadIO (..)
  , MonadTrans (..)
  , Void
  , runGame
  , runGamePartial
  , (>\\)
  ) where

import Control.Monad.Trans
import Control.Monad.Morph

import Data.Void

data Game i o m r
  = Pure r
  | Lift (m (Game i o m r))
  | Request o (i -> Game i o m r)

-- type Game' i o m r = FreeT (CofreeF ((->) i) o) m r

instance Functor m => Functor (Game i o m) where
  fmap f (Lift cont) = Lift $ (fmap.fmap) f cont
  fmap f (Pure r)    = Pure (f r)

instance Functor m => Applicative (Game i o m) where
  pure = Pure
  (<*>) = undefined

instance Functor m => Monad (Game i o m) where
  Pure r >>= f = f r
  Lift cont >>= f = Lift $ (>>=f) <$> cont
  Request o cont >>= f = Request o ((>>=f) . cont)

instance MonadTrans (Game i o) where
  lift f = Lift (Pure <$> f)

instance MFunctor (Game i o) where
  hoist f = go
    where go (Pure r) = Pure r
          go (Request o cont) = Request o (go . cont)
          go (Lift cont) = Lift $ f (go <$> cont)

instance MMonad (Game i o) where
  embed f = go
    where go (Pure r) = Pure r
          go (Request o cont) = Request o (go . cont)
          go (Lift cont) = f cont >>= go

instance MonadIO m => MonadIO (Game i o m) where
  liftIO = lift . liftIO

instance (Functor m, Semigroup r) => Semigroup (Game i o m r) where
  g1 <> g2 = go g1 where
    go (Pure r) = fmap (r <>) g2
    go (Lift m) = Lift $ go <$> m
    go (Request o cont) = Request o (go . cont)

instance (Functor m, Monoid r) => Monoid (Game i o m r) where
  mempty = Pure mempty

nest :: Functor m
     => (i -> i')
     -> (o' -> o)
     -> Game i' o' m r
     -> Game i  o  m r
nest fIn fOut = go
  where go (Pure r) = Pure r
        go (Lift cont) = Lift $ go <$> cont
        go (Request o cont) = Request (fOut o) (go . cont . fIn)

filterIn
  :: Functor m
  => (i -> Maybe i')
  -> Game i' o m r
  -> Game i o m r
filterIn f = go where
  go (Pure r) = Pure r
  go (Lift m) = Lift $ go <$> m
  go (Request o cont) = Request o $ \i -> case f i of
    Just i' -> go $ cont i'
    Nothing -> go $ Request o cont

(>\\)
  :: Functor m
  => (o' -> Game i o m i')
  -> Game i' o' m r
  -> Game i o m r
fg >\\ g' = go g' where
  go g = case g of
           Pure r -> Pure r
           Lift m -> Lift $ go <$> m
           Request o' cont -> fg o' >>= go . cont

request :: o -> Game a o m a
request o = Request o Pure

await :: Game a () m a
await = request ()

runGame :: Monad m => Game Void Void m r -> m r
runGame (Pure r) = pure r
runGame (Request v _) = absurd v
runGame (Lift m) = m >>= runGame

-- runGamePartial = undefined
runGamePartial :: Monad m => (forall a. r -> m a) -> i -> Game i o m r -> m (Game i o m r, o)
runGamePartial f i = go where
  go (Pure r) = f r
  go (Lift m) = m >>= go
  go (Request o cont) = pure $ (cont i, o)

-- Lib --
mapIn :: Functor m
      => (i -> i')
      -> Game i' o m r
      -> Game i  o m r
mapIn f = nest f id

mapOut :: Functor m
       => (o' -> o)
       -> Game i o' m r
       -> Game i o  m r
mapOut = nest id

-- Time
integrateTime :: Game i t m r -> Game i (t,t) m r
integrateTime = undefined
