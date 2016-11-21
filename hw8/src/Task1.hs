{-# LANGUAGE NoImplicitPrelude #-}
module Task1 where

import           Data.Monoid (Monoid (..), (<>))
import           Prelude     (Either (..), ($))

class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b

class MonadTrans t where
  lift :: Monad m => m a -> t m a


newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance (Monad m) => Monad (StateT s m) where
  return a = StateT $ \s -> return (a, s)
  m >>= f  = StateT $ \s -> runStateT m s >>=
                            \(a, s') -> runStateT (f a) s'

instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> m >>=
                          \a -> return (a, s)


newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

instance (Monoid w, Monad m) => Monad (WriterT w m) where
  return a = WriterT $ return (a, mempty)
  m >>= f  = WriterT $ runWriterT m >>=
                       \(a, w)  -> runWriterT (f a) >>=
                                   \(b, w') -> return (b, w <> w')

instance (Monoid w) => MonadTrans (WriterT w) where
  lift m = WriterT $ m >>=
                     \a -> return (a, mempty)


newtype EitherT l m r = EitherT {runEitherT :: m (Either l r) }

instance (Monad m) => Monad (EitherT l m) where
  return a = EitherT $ return (Right a)
  m >>= f  = EitherT $ runEitherT m >>=
                       \t -> case t of
                               Left l  -> return (Left l)
                               Right r -> runEitherT $ f r

instance MonadTrans (EitherT l) where
  lift m = EitherT $ m >>=
                     \a -> return (Right a)
