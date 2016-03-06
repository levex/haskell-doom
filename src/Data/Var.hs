{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.Var where
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef
import Foreign

-- Getter
class Monad m => HasGetter m g a | g -> a where
    get :: g -> m a

instance (MonadIO m, MonadReader e m, HasGetter m g a) => HasGetter m (e -> g) a where
    get a = do
        e <- asks a
        get e

instance MonadIO m => HasGetter m (IORef a) a where
    get = liftIO . readIORef

instance (MonadIO m, Storable a) => HasGetter m (Ptr a) a where
    get = liftIO . peek

-- Setter
class Monad m => HasSetter m s a | s -> a where
    ($=) :: s -> a -> m ()
    put  :: s -> a -> m ()
    put = ($=!)
    ($=!) :: s -> a -> m ()
    s $=! a = a `seq` (s $= a)

instance (MonadIO m, MonadReader e m, HasSetter m g a) => HasSetter m (e -> g) a where
    s $= a
        = asks s >>= ($=! a)

instance MonadIO m => HasSetter m (IORef a) a where
    ($=) = (.) liftIO . writeIORef

instance (MonadIO m, Storable a) => HasSetter m (Ptr a) a where
    ($=) = (.) liftIO . poke

-- Combinators
-- Modify
($~) :: (HasGetter m t a, HasSetter m t a) => t -> (a -> a) -> m ()
($~) a f = do
    e <- get a
    a $=! f e

-- Add
(+=) :: (Num a, HasGetter m t a, HasSetter m t a)  => t -> a -> m ()
a += w = a $~ (+ w)

-- Subtract
(-=) :: (Num a, HasGetter m t a, HasSetter m t a)  => t -> a -> m ()
a -= w = a $~ subtract w

-- Multiply
(*=) :: (Num a, HasGetter m t a, HasSetter m t a)  => t -> a -> m ()
a *= w = a $~ (* w)
