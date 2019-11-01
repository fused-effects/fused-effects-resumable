{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}

-- | Provides a carrier for 'Resumable' that can, given a handler function, resume the computation that threw an exception.
module Control.Carrier.Resumable.Resume
( -- * Resumable carrier
  runResumable
, ResumableC(..)
  -- * Resumable effect
, module Control.Effect.Resumable
) where

import Control.Applicative (Alternative(..))
import Control.Algebra
import Control.Carrier.Reader
import Control.Effect.Resumable
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Run a 'Resumable' effect, resuming uncaught errors with a given handler.
--
--   Note that this may be less efficient than defining a specialized carrier type and instance specifying the handler’s behaviour directly. Performance-critical code may wish to do that to maximize the opportunities for fusion and inlining.
--
-- @
-- 'runResumable' f ('pure' a) = 'pure' a
-- @
-- @
-- 'runResumable' f ('throwResumable' e) = f e
-- @
--
-- @since 0.1.0.0
runResumable
  :: (forall x . err x -> m x)
  -> ResumableC err m a
  -> m a
runResumable with = runReader (Handler with) . runResumableC

-- | @since 0.1.0.0
newtype ResumableC err m a = ResumableC { runResumableC :: ReaderC (Handler err m) m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans (ResumableC err) where
  lift = ResumableC . lift
  {-# INLINE lift #-}

newtype Handler err m = Handler { runHandler :: forall x . err x -> m x }

instance Algebra sig m => Algebra (Resumable err :+: sig) (ResumableC err m) where
  alg (L (Resumable err k)) = ResumableC (ReaderC (\ handler -> runHandler handler err)) >>= k
  alg (R other)             = ResumableC (alg (R (handleCoercible other)))
  {-# INLINE alg #-}
