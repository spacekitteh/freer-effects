{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
-- {-# LANGUAGE PartialTypeSignatures #-}
-- |
-- Module:       Data.FTCQueue
-- Description:  Fast type-aligned queue optimized to effectful functions.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.
-- License:      BSD3
-- Maintainer:   ixcom-core@ixperta.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- * Constant-time append\/('><') and snoc\/('|>')
-- * Average constant-time 'viewL' (left-edge deconstruction).
--
-- Using <http://okmij.org/ftp/Haskell/extensible/FTCQueue1.hs> as a starting
-- point.
--
-- A minimal version of FTCQueue from "Reflection w/o Remorse":
--
-- * Research: <http://okmij.org/ftp/Haskell/Reflection.html>
-- * <https://hackage.haskell.org/package/type-aligned type-aligned> (FTCQueue)
module Data.FTCQueue
    ( FTCQueue
    , tsingleton
    , (|>)
    , snoc
    , (><)
    , append
    , ViewL(..)
    , tviewl, cons
    )
  where

import Data.Function (flip)
import Control.Category (Category(id, (.)))
import Control.Arrow (Arrow(arr,(***)))


-- | Non-empty tree. Deconstruction operations make it more and more
-- left-leaning
data FTCQueue arr a b where
    Leaf :: arr a b -> FTCQueue arr a b
    Node :: FTCQueue arr a x -> FTCQueue arr x b -> FTCQueue arr a b

-- | Build a leaf from a single operation. [O(1)]
tsingleton :: arr a b -> FTCQueue arr a b
tsingleton = Leaf
{-# INLINE tsingleton #-}

-- | Append an operation to the right of the tree. [O(1)]
(|>) :: FTCQueue arr a x -> arr x b -> FTCQueue arr a b
t |> r = Node t (Leaf r)
{-# INLINE (|>) #-}

(<|) :: arr a b -> FTCQueue arr b c -> FTCQueue arr a c
t <| r = Node (Leaf t) r
{-# INLINE (<|) #-}

cons :: arr a b -> FTCQueue arr b c -> FTCQueue arr a c
cons = (<|)
{-# INLINE cons #-}

-- | An alias for '(|>)'
snoc :: FTCQueue arr a x -> arr x b -> FTCQueue arr a b
snoc = (|>)
{-# INLINE snoc #-}

-- | Append two trees of operations. [O(1)]
(><)   :: FTCQueue arr a x -> FTCQueue arr x b -> FTCQueue arr a b
t1 >< t2 = Node t1 t2
{-# INLINE (><) #-}

-- | An alias for '(><)'
append :: FTCQueue arr a x -> FTCQueue arr x b -> FTCQueue arr a b
append = (><)
{-# INLINE append #-}

-- | Left view deconstruction data structure.
data ViewL arr a b where
    TOne  :: arr a b -> ViewL arr a b
    (:|)  :: arr a x -> FTCQueue arr x b -> ViewL arr a b

-- | Left view deconstruction. [average O(1)]
tviewl :: FTCQueue arr a b -> ViewL arr a b
tviewl (Leaf r)     = TOne r
tviewl (Node t1 t2) = go t1 t2
  where
    go :: FTCQueue arr a x -> FTCQueue arr x b -> ViewL arr a b
    go (Leaf r)       tr = r :| tr
    go (Node tl1 tl2) tr = go tl1 (Node tl2 tr)

instance (Category arr) => Category (ViewL arr) where
  id = TOne id
  (TOne a) . (TOne b) = ( b) :|  (Leaf a)
  (TOne a) . (x :| y) =  x :| (y |> a )
  (x :| y) . (TOne b) = b :| (append (tsingleton x) y)
  (x :| y) . (a :| b) = a :| (append (b |> x) y)

instance Arrow arr => Arrow (ViewL arr) where
  arr = TOne . arr
  (TOne a) *** (TOne b) = TOne (a *** b)
  (TOne a) *** (x :| y) = (a*** x) :| (id *** y)
  (x :| y) *** (TOne b) =  (x *** ( b)) :| (y *** id)
  (x :| y) *** (a :| b) = (x *** a) :| (y *** b)

{-instance ArrowApply arr => ArrowApply (ViewL arr) where
  -- app :: ViewL arr (ViewL arr a b, a) b
  app = TOne ( arr go ) where
    go :: forall arr a b . ArrowApply arr =>  (ViewL arr a b, a) -> b
    go (TOne (f :: arr a b), a)= _ f a (app @arr(arr , a))---_ f (pure @(ArrowMonad arr) a)
-}
instance Category arr => Category (FTCQueue arr) where
  id = Leaf id
  (.) = flip append
  
instance Arrow arr => Arrow (FTCQueue arr) where
  arr = Leaf . arr
  (***)  f' g' = case (tviewl f', tviewl g') of
    (TOne f, TOne g) -> Leaf (f *** g)
    (TOne f, (a :| b)) -> (Leaf f) *** (a <| b)
    ((x :| y), TOne g) -> (x <| y) *** (Leaf g)
    ((x :| y), (a :| b)) -> (x <| y) *** (a <| b)




