{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:       Control.Monad.Freer.Coroutine
-- Description:  Composable coroutine effects layer.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.
-- License:      BSD3
-- Maintainer:   ixcom-core@ixperta.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- An effect to compose functions with the ability to yield.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.Coroutine
    (
    -- * Yield Control
      Yield(..)
    , yield

    -- * Handle Yield Effect
    , Status(..)
    , runC
    , interposeC
    , replyC
    )
  where

import Control.Applicative (pure)
import Data.Function (($))
import Data.Functor (Functor)
import Control.Category
import Control.Arrow (Kleisli (..), arr, Arrow(..))
import Control.Monad.Freer.Internal (Eff, Member, handleRelay, interpose, send)


-- | A type representing a yielding of control.
--
-- Type variables have following meaning:
--
-- [@a@]
--   The current type.
--
-- [@b@]
--   The input to the continuation function.
--
-- [@c@]
--   The output of the continuation.
data Yield arr a b c = Yield a (arr b c)
  deriving (Functor)

-- | Lifts a value and a function into the Coroutine effect.
yield :: Arrow arr => Member (Yield arr a b) effs => a -> arr b c -> Eff effs arr c
yield x f = send (Yield x ( f))

-- | Represents status of a coroutine.
data Status effs (arr :: * -> * -> *) a b r
    = Done r
    -- ^ Coroutine is done with a result value of type @r@.
    | Continue a (arr b (Eff effs arr (Status effs arr a b r)))
    -- ^ Reporting a value of the type @a@, and resuming with the value of type
    -- @b@, possibly ending with a value of type @x@.

-- | Reply to a coroutine effect by returning the Continue constructor.
replyC
  :: Category arr => Yield arr a b c
  -> (arr c (Eff effs arr (Status effs arr a b r)))
  -> Eff effs arr (Status effs arr a b r)
replyC (Yield a k) arr = pure $ Continue a (arr . k)

-- | Launch a coroutine and report its status.
runC :: Eff (Yield arr a b ': effs) arr r -> Eff effs arr (Status effs arr a b r)
runC = handleRelay (pure . Done) (  replyC)

-- | Launch a coroutine and report its status, without handling (removing)
-- 'Yield' from the typelist. This is useful for reducing nested coroutines.
interposeC
    :: Member (Yield arr a b) effs
    => Eff effs arr r
    -> Eff effs arr (Status effs arr a b r)
interposeC = interpose (pure . Done) replyC
