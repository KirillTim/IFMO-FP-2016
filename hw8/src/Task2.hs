{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
module Task2 where

import           Control.Monad.Trans.Class  (MonadTrans (..))
import           Control.Monad.Trans.Maybe  (MaybeT (..))
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Control.Monad.Trans.State  (StateT (..))
import           Prelude                    (Monad (..), ($))

class Monad m => MonadState s m where
    get :: m s
    put :: s -> m ()

instance (Monad m) => MonadState s (StateT s m) where
    get   = StateT $ \s -> return (s, s)
    put s = StateT $ \_ -> return ((), s)

instance (MonadState s m) => MonadState s (ReaderT r m) where
    get   = lift get
    put s = lift (put s)

instance (MonadState s m) => MonadState s (MaybeT m) where
    get   = lift get
    put s = lift (put s)


