{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Stack where

import Control.Applicative
import Control.Monad.Fail
import Control.Monad.Trans
import Control.Monad.RWS.Lazy


pop :: (MonadState [a] m, MonadFail m) => m a
pop = do
    x:xs <- get
    put xs
    pure x

push :: MonadState [a] m => a -> m ()
push x = state $ \xs -> ((), x:xs)

pushn :: MonadState [a] m => [a] -> m ()
pushn ys = state $ \xs -> ((), ys ++ xs)

peek :: (MonadState [a] m, MonadFail m) => m a
peek = do
    x <- pop
    push x
    pure x

isempty :: MonadState [a] m => m Bool
isempty = state $ \case
    x:xs -> (False, x:xs)
    []   -> (True, [])

trypop :: (Eq a, MonadState [a] m, MonadFail m) => a -> m Bool
trypop x = do
    e <- isempty
    if e
    then pure False
    else do
        y <- peek
        if x == y
        then pop >> pure True
        else pure False

left :: (Monoid w, Monad m) => RWST r w q m a -> RWST r w (q, s) m a
left s = RWST $ \w (l, r) -> do
    (a, l', w) <- runRWST s w l
    pure (a, (l', r), w)

right :: (Monoid w, Monad m) => RWST r w s m a -> RWST r w (q, s) m a
right s = RWST $ \w (l, r) -> do
    (a, r', w) <- runRWST s w r
    pure (a, (l, r'), w)