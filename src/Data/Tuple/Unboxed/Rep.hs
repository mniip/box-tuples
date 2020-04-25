{-# LANGUAGE UnboxedTuples, MagicHash, CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TypeInType, StarIsType, RankNTypes, MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -fobject-code #-}

-------------------------------------------------------------------------------
-- |
-- Module      : Data.Tuple.Unboxed.Rep
-- Copyright   : (C) 2020 mniip
-- License     : MIT
-- Maintainer  : mniip <mniip@mniip.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- This module provides an ad-hoc polymorphic alternative to the functions in 
-- "Data.Tuple.Unboxed". The functions have the same basic signature, but using
-- GHC 8.0+ features they are made parametrically polymorphic within each fixed
-- representation/calling convention.
--
-- This module defines instances for all calling conventions that have ever come
-- up in "GHC.Prim" but in principle other cases could be added too.
-------------------------------------------------------------------------------
module Data.Tuple.Unboxed.Rep
  ( RepCompose1(..)
  , RepCompose2(..)
  , RepCompose3(..)
  , RepCompose4(..)
  ) where

import Data.Kind
import GHC.Prim
import GHC.Exts

#if __GLASGOW_HASKELL__ >= 802
type VoidRep = TupleRep '[]
#endif

#if __GLASGOW_HASKELL__ < 802
type LiftedRep = PtrRepLifted
type UnlfitedRep = PtrRepUnlifted
#endif

class RepCompose1 (r0 :: RuntimeRep) (r1 :: RuntimeRep) where
  repCompose1 :: forall (a :: TYPE r0) (b :: TYPE r1) (r :: *). (a -> (# b #)) -> a -> (b -> r) -> r
  repDecompose1 :: forall (a :: TYPE r0) (b :: TYPE r1) (r :: *). (a -> (b -> (# b #)) -> (# b #)) -> a -> (# b #)

class RepCompose2 (r0 :: RuntimeRep) (r1 :: RuntimeRep) (r2 :: RuntimeRep) where
  repCompose2 :: forall (a :: TYPE r0) (b :: TYPE r1) (c :: TYPE r2) (r :: *). (a -> (# b, c #)) -> a -> (b -> c -> r) -> r
  repDecompose2 :: forall (a :: TYPE r0) (b :: TYPE r1) (c :: TYPE r2) (r :: *). (a -> (b -> c -> (# b, c #)) -> (# b, c #)) -> a -> (# b, c #)

class RepCompose3 (r0 :: RuntimeRep) (r1 :: RuntimeRep) (r2 :: RuntimeRep) (r3 :: RuntimeRep) where
  repCompose3 :: forall (a :: TYPE r0) (b :: TYPE r1) (c :: TYPE r2) (d :: TYPE r3) (r :: *). (a -> (# b, c, d #)) -> a -> (b -> c -> d -> r) -> r
  repDecompose3 :: forall (a :: TYPE r0) (b :: TYPE r1) (c :: TYPE r2) (d :: TYPE r3) (r :: *). (a -> (b -> c -> d -> (# b, c, d #)) -> (# b, c, d #)) -> a -> (# b, c, d #)

class RepCompose4 (r0 :: RuntimeRep) (r1 :: RuntimeRep) (r2 :: RuntimeRep) (r3 :: RuntimeRep) (r4 :: RuntimeRep) where
  repCompose4 :: forall (a :: TYPE r0) (b :: TYPE r1) (c :: TYPE r2) (d :: TYPE r3) (e :: TYPE r4) (r :: *). (a -> (# b, c, d, e #)) -> a -> (b -> c -> d -> e -> r) -> r
  repDecompose4 :: forall (a :: TYPE r0) (b :: TYPE r1) (c :: TYPE r2) (d :: TYPE r3) (e :: TYPE r4) (r :: *). (a -> (b -> c -> d -> e -> (# b, c, d, e #)) -> (# b, c, d, e #)) -> a -> (# b, c, d, e #)

instance RepCompose1 LiftedRep LiftedRep where
  repCompose1 f x k = case f x of (# a #) -> k a
  repDecompose1 f x = f x (\a -> (# a #))

instance RepCompose1 UnliftedRep LiftedRep where
  repCompose1 f x k = case f x of (# a #) -> k a
  repDecompose1 f x = f x (\a -> (# a #))

instance RepCompose1 IntRep LiftedRep where
  repCompose1 f x k = case f x of (# a #) -> k a
  repDecompose1 f x = f x (\a -> (# a #))

instance RepCompose1 WordRep LiftedRep where
  repCompose1 f x k = case f x of (# a #) -> k a
  repDecompose1 f x = f x (\a -> (# a #))

instance RepCompose1 AddrRep LiftedRep where
  repCompose1 f x k = case f x of (# a #) -> k a
  repDecompose1 f x = f x (\a -> (# a #))

instance RepCompose2 VoidRep VoidRep LiftedRep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))

instance RepCompose2 VoidRep VoidRep UnliftedRep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))

instance RepCompose2 VoidRep VoidRep IntRep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))

instance RepCompose2 VoidRep VoidRep WordRep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))

instance RepCompose2 VoidRep VoidRep AddrRep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))

instance RepCompose2 VoidRep VoidRep FloatRep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))

instance RepCompose2 VoidRep VoidRep DoubleRep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))

instance RepCompose2 LiftedRep UnliftedRep UnliftedRep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))

instance RepCompose2 UnliftedRep IntRep UnliftedRep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))

instance RepCompose2 IntRep IntRep LiftedRep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))

instance RepCompose2 IntRep IntRep UnliftedRep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))

instance RepCompose2 IntRep IntRep IntRep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))

instance RepCompose2 WordRep IntRep UnliftedRep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))

instance RepCompose2 WordRep WordRep IntRep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))

instance RepCompose2 WordRep WordRep WordRep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))

instance RepCompose2 AddrRep IntRep UnliftedRep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))

instance RepCompose2 FloatRep IntRep IntRep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))

instance RepCompose2 DoubleRep IntRep IntRep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))

instance RepCompose3 VoidRep VoidRep LiftedRep LiftedRep where
  repCompose3 f x k = case f x of (# a, b, c #) -> k a b c
  repDecompose3 f x = f x (\a b c -> (# a, b, c #))

instance RepCompose3 VoidRep VoidRep UnliftedRep AddrRep where
  repCompose3 f x k = case f x of (# a, b, c #) -> k a b c
  repDecompose3 f x = f x (\a b c -> (# a, b, c #))

instance RepCompose3 VoidRep VoidRep IntRep LiftedRep where
  repCompose3 f x k = case f x of (# a, b, c #) -> k a b c
  repDecompose3 f x = f x (\a b c -> (# a, b, c #))

instance RepCompose3 VoidRep VoidRep IntRep IntRep where
  repCompose3 f x k = case f x of (# a, b, c #) -> k a b c
  repDecompose3 f x = f x (\a b c -> (# a, b, c #))

instance RepCompose3 VoidRep VoidRep AddrRep WordRep where
  repCompose3 f x k = case f x of (# a, b, c #) -> k a b c
  repDecompose3 f x = f x (\a b c -> (# a, b, c #))

instance RepCompose3 LiftedRep AddrRep UnliftedRep UnliftedRep where
  repCompose3 f x k = case f x of (# a, b, c #) -> k a b c
  repDecompose3 f x = f x (\a b c -> (# a, b, c #))

instance RepCompose3 IntRep IntRep IntRep IntRep where
  repCompose3 f x k = case f x of (# a, b, c #) -> k a b c
  repDecompose3 f x = f x (\a b c -> (# a, b, c #))

instance RepCompose3 FloatRep IntRep IntRep UnliftedRep where
  repCompose3 f x k = case f x of (# a, b, c #) -> k a b c
  repDecompose3 f x = f x (\a b c -> (# a, b, c #))

instance RepCompose3 DoubleRep IntRep IntRep UnliftedRep where
  repCompose3 f x k = case f x of (# a, b, c #) -> k a b c
  repDecompose3 f x = f x (\a b c -> (# a, b, c #))

instance RepCompose3 DoubleRep IntRep IntRep IntRep where
  repCompose3 f x k = case f x of (# a, b, c #) -> k a b c
  repDecompose3 f x = f x (\a b c -> (# a, b, c #))

instance RepCompose4 VoidRep VoidRep IntRep IntRep IntRep where
  repCompose4 f x k = case f x of (# a, b, c, d #) -> k a b c d
  repDecompose4 f x = f x (\a b c d -> (# a, b, c, d #))

instance RepCompose4 UnliftedRep IntRep UnliftedRep IntRep UnliftedRep where
  repCompose4 f x k = case f x of (# a, b, c, d #) -> k a b c d
  repDecompose4 f x = f x (\a b c d -> (# a, b, c, d #))

instance RepCompose4 DoubleRep IntRep WordRep WordRep IntRep where
  repCompose4 f x k = case f x of (# a, b, c, d #) -> k a b c d
  repDecompose4 f x = f x (\a b c d -> (# a, b, c, d #))

#if __GLASGOW_HASKELL__ >= 808
instance RepCompose2 VoidRep VoidRep Int64Rep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))

instance RepCompose2 VoidRep VoidRep Word64Rep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))

instance RepCompose2 Int8Rep Int8Rep Int8Rep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))

instance RepCompose2 Int16Rep Int16Rep Int16Rep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))

instance RepCompose2 Int64Rep IntRep UnliftedRep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))

instance RepCompose2 Int64Rep Int64Rep Int64Rep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))

instance RepCompose2 Word8Rep Word8Rep Word8Rep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))

instance RepCompose2 Word16Rep Word16Rep Word16Rep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))

instance RepCompose2 Word64Rep IntRep UnliftedRep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))

instance RepCompose2 Word64Rep Word64Rep Word64Rep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))

instance RepCompose2 DoubleRep Int64Rep IntRep where
  repCompose2 f x k = case f x of (# a, b #) -> k a b
  repDecompose2 f x = f x (\a b -> (# a, b #))
#endif

#else
module Data.Tuple.Unboxed.Rep () where
#endif
