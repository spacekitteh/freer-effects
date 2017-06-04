{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
    , tviewl
    )
  where

import Control.Arrow

-- | Non-empty tree. Deconstruction operations make it more and more
-- left-leaning
data FTCQueue arr a b where
    Leaf :: Arrow arr => arr a b -> FTCQueue arr a b
    Node :: Arrow arr => FTCQueue arr a x -> FTCQueue arr x b -> FTCQueue arr a b

-- | Build a leaf from a single operation. [O(1)]
tsingleton :: arr a b -> FTCQueue arr a b
tsingleton = Leaf
{-# INLINE tsingleton #-}

-- | Append an operation to the right of the tree. [O(1)]
(|>) :: Arrow arr => FTCQueue arr a x -> arr x b -> FTCQueue arr a b
t |> r = Node t (Leaf r)
{-# INLINE (|>) #-}

-- | An alias for '(|>)'
snoc :: Arrow arr => FTCQueue arr a x -> arr x b -> FTCQueue arr a b
snoc = (|>)
{-# INLINE snoc #-}

-- | Append two trees of operations. [O(1)]
(><)   :: Arrow arr => FTCQueue arr a x -> FTCQueue arr x b -> FTCQueue arr a b
t1 >< t2 = Node t1 t2
{-# INLINE (><) #-}

-- | An alias for '(><)'
append :: Arrow arr => FTCQueue arr a x -> FTCQueue arr x b -> FTCQueue arr a b
append = (><)
{-# INLINE append #-}

-- | Left view deconstruction data structure.
data ViewL arr a b where
    TOne  :: Arrow arr => arr a b -> ViewL arr a b
    (:|)  :: Arrow arr => arr a x -> FTCQueue arr x b -> ViewL arr a b

-- | Left view deconstruction. [average O(1)]
tviewl :: Arrow arr => FTCQueue arr a b -> ViewL arr a b
tviewl (Leaf r)     = TOne r
tviewl (Node t1 t2) = go t1 t2
  where
    go :: Arrow arr => FTCQueue arr a x -> FTCQueue arr x b -> ViewL arr a b
    go (Leaf r)       tr = r :| tr
    go (Node tl1 tl2) tr = go tl1 (Node tl2 tr)
