{-# LANGUAGE UnboxedTuples, MagicHash, CPP #-}
{-# OPTIONS_GHC -fobject-code #-}
#include "MachDeps.h"
-------------------------------------------------------------------------------
-- |
-- Module      : Data.Tuple.Unboxed
-- Copyright   : (C) 2016,2020 mniip
-- License     : MIT
-- Maintainer  : mniip <mniip@mniip.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- As of now, the GHCi interactive runtime is incapable of working on unboxed
-- tuples. In particular, it is unable to fully apply any function returning an
-- unboxed tuple, create a function that takes a non-nullary unboxed tuple as
-- argument, or pass a non-nullary tuple to some other function. The usual
-- solution is to enable object code generation with @-fobject-code@. This
-- module serves as a workaround for the cases where @-fobject-code@ is
-- undesiable.
--
-- Due to the aforementioned restrictions on operations on unboxed tuples, we
-- can't simply expose operations of type @(\# a, b \#) -> (a, b)@. We have to
-- provide operations for working on functions of type @a -> (\# b, c \#)@.
--
-- More often than not, the types in unboxed tuples are themselves unboxed.
-- There are two major issues with this: 1) we cannot use @(,)@ and 2) we have to
-- provide different functions for different combinations of types. It is also
-- worth mentioning that the 'a' above can also be unboxed and therefore cannot
-- be quantified either.
--
-- This module provides a class of @compose@ functions of type
--
-- > composeA#BC :: (a -> (# b, c, ... #)) -> a -> (b -> c -> ... -> r) -> r
--
-- Where @A@ is the function argument type 'a', @#@ is number of fields in the
-- tuple, and @BC@ are the tupled types 'b', 'c', ...
-- 
-- The key for type names is as follows:
--
--  * _ = @forall (a :: *). a@ (regular lifted type)
--  * a = 'Array#'
--  * Aa = 'ArrayArray#'
--  * b = 'ByteArray#'
--  * c = 'Char#'
--  * C = 'Compact#'
--  * d = 'Double#'
--  * f = 'Float#'
--  * i = 'Int#'
--  * li = INT32 (@if WORD\_SIZE_IN_BITS >= 32 then 'Int#' else 'Int32#'@)
--  * lli = INT64 (@if WORD\_SIZE_IN_BITS >= 64 then 'Int#' else 'Int64#'@)
--  * hi = 'Int16#'
--  * hhi = 'Int8#'
--  * Ma = 'MutableArray#'
--  * MAa = 'MutableArrayArray#'
--  * Mb = 'MutableByteArray#'
--  * Mv = 'MVar#'
--  * o = 'BCO#'
--  * p = 'Addr#'
--  * s = 'State#'
--  * Sa = 'SmallArray#'
--  * SMa = 'SmallMutableArray#'
--  * Sn = 'StableName#'
--  * Sp = 'StablePtr#'
--  * t = 'ThreadId#'
--  * Tv = 'TVar#'
--  * v = 'MutVar#'
--  * w = 'Word#'
--  * lw = WORD32 (@if WORD\_SIZE_IN_BITS >= 32 then 'Word#' else 'Word32#'@)
--  * llw = WORD64 (@if WORD\_SIZE_IN_BITS >= 64 then 'Word#' else 'Word64#'@)
--  * hw = 'Word16#'
--  * hhw = 'Word8#'
--  * Wp = 'Weak#'
--
-- This module contains functions for all combinations of types that have ever
-- come up in "GHC.Prim" but in principle other cases could be added too.
-------------------------------------------------------------------------------
module Data.Tuple.Unboxed where

import GHC.Prim

#if WORD_SIZE_IN_BITS >= 32
#define INT32 Int#
#define WORD32 Word#
#else
#define INT32 Int32#
#define WORD32 Word32#
#endif
#if WORD_SIZE_IN_BITS >= 64
#define INT64 Int#
#define WORD64 Word#
#else
#define INT64 Int64#
#define WORD64 Word64#
#endif

compose_1_ :: (a -> (# b #)) -> a -> (b -> r) -> r
composei1_ :: (Int# -> (# a #)) -> Int# -> (a -> r) -> r
composeo1_ :: (BCO# -> (# a #)) -> BCO# -> (a -> r) -> r
composep1_ :: (Addr# -> (# a #)) -> Addr# -> (a -> r) -> r
composew1_ :: (Word# -> (# a #)) -> Word# -> (a -> r) -> r
compose_2ab :: (a -> (# Array# e, ByteArray# #)) -> a -> (Array# e -> ByteArray# -> r) -> r
composeb2ib :: (ByteArray# -> (# Int#, ByteArray# #)) -> ByteArray# -> (Int# -> ByteArray# -> r) -> r
composed2ii :: (Double# -> (# INT64, Int# #)) -> Double# -> (Int# -> Int# -> r) -> r
composed2llii :: (Double# -> (# INT64, Int# #)) -> Double# -> (INT64 -> Int# -> r) -> r
composef2ii :: (Float# -> (# Int#, Int# #)) -> Float# -> (Int# -> Int# -> r) -> r
composei2i_ :: (Int# -> (# Int#, a #)) -> Int# -> (Int# -> a -> r) -> r
composei2ib :: (Int# -> (# Int#, ByteArray# #)) -> Int# -> (Int# -> ByteArray# -> r) -> r
composei2ii :: (Int# -> (# Int#, Int# #)) -> Int# -> (Int# -> Int# -> r) -> r
composeli2ib :: (INT32 -> (# Int#, ByteArray# #)) -> INT32 -> (Int# -> ByteArray# -> r) -> r
composelli2ib :: (INT64 -> (# Int#, ByteArray# #)) -> INT64 -> (Int# -> ByteArray# -> r) -> r
composellw2ib :: (WORD64 -> (# Int#, ByteArray# #)) -> WORD64 -> (Int# -> ByteArray# -> r) -> r
composelw2ib :: (WORD32 -> (# Int#, ByteArray# #)) -> WORD32 -> (Int# -> ByteArray# -> r) -> r
composep2ib :: (Addr# -> (# Int#, ByteArray# #)) -> Addr# -> (Int# -> ByteArray# -> r) -> r
composes2s_ :: (State# s -> (# State# t, a #)) -> State# s -> (State# t -> a -> r) -> r
composes2sa :: (State# s -> (# State# t, Array# e #)) -> State# s -> (State# t -> Array# e -> r) -> r
composes2sb :: (State# s -> (# State# t, ByteArray# #)) -> State# s -> (State# t -> ByteArray# -> r) -> r
composes2sc :: (State# s -> (# State# t, Char# #)) -> State# s -> (State# t -> Char# -> r) -> r
composes2sd :: (State# s -> (# State# t, Double# #)) -> State# s -> (State# t -> Double# -> r) -> r
composes2sf :: (State# s -> (# State# t, Float# #)) -> State# s -> (State# t -> Float# -> r) -> r
composes2si :: (State# s -> (# State# t, Int# #)) -> State# s -> (State# t -> Int# -> r) -> r
composes2sli :: (State# s -> (# State# t, INT32 #)) -> State# s -> (State# t -> INT32 -> r) -> r
composes2slli :: (State# s -> (# State# t, INT64 #)) -> State# s -> (State# t -> INT64 -> r) -> r
composes2sllw :: (State# s -> (# State# t, WORD64 #)) -> State# s -> (State# t -> WORD64 -> r) -> r
composes2slw :: (State# s -> (# State# t, WORD32 #)) -> State# s -> (State# t -> WORD32 -> r) -> r
composes2sMa :: (State# s -> (# State# t, MutableArray# u e #)) -> State# s -> (State# t -> MutableArray# u e -> r) -> r
composes2sMb :: (State# s -> (# State# t, MutableByteArray# u #)) -> State# s -> (State# t -> MutableByteArray# u -> r) -> r
composes2sMv :: (State# s -> (# State# t, MVar# u e #)) -> State# s -> (State# t -> MVar# u e -> r) -> r
composes2so :: (State# s -> (# State# t, BCO# #)) -> State# s -> (State# t -> BCO# -> r) -> r
composes2sp :: (State# s -> (# State# t, Addr# #)) -> State# s -> (State# t -> Addr# -> r) -> r
composes2sSn :: (State# s -> (# State# t, StableName# a #)) -> State# s -> (State# t -> StableName# a -> r) -> r
composes2sSp :: (State# s -> (# State# t, StablePtr# a #)) -> State# s -> (State# t -> StablePtr# a -> r) -> r
composes2st :: (State# s -> (# State# t, ThreadId# #)) -> State# s -> (State# t -> ThreadId# -> r) -> r
composes2sTv :: (State# s -> (# State# t, TVar# u e #)) -> State# s -> (State# t -> TVar# u e -> r) -> r
composes2sv :: (State# s -> (# State# t, MutVar# u e #)) -> State# s -> (State# t -> MutVar# u e -> r) -> r
composes2sw :: (State# s -> (# State# t, Word# #)) -> State# s -> (State# t -> Word# -> r) -> r
composes2sWp :: (State# s -> (# State# t, Weak# a #)) -> State# s -> (State# t -> Weak# a -> r) -> r
composew2ib :: (Word# -> (# Int#, ByteArray# #)) -> Word# -> (Int# -> ByteArray# -> r) -> r
composew2wi :: (Word# -> (# Word#, Int# #)) -> Word# -> (Word# -> Int# -> r) -> r
composew2ww :: (Word# -> (# Word#, Word# #)) -> Word# -> (Word# -> Word# -> r) -> r
compose_3pab :: (a -> (# Addr#, Array# e, ByteArray# #)) -> a -> (Addr# -> Array# e -> ByteArray# -> r) -> r
compose_3pba :: (a -> (# Addr#, ByteArray#, Array# e #)) -> a -> (Addr# -> ByteArray# -> Array# e -> r) -> r
composed3iib :: (Double# -> (# Int#, Int#, ByteArray# #)) -> Double# -> (Int# -> Int# -> ByteArray# -> r) -> r
composed3iii :: (Double# -> (# Int#, Int#, Int# #)) -> Double# -> (Int# -> Int# -> Int# -> r) -> r
composef3iib :: (Float# -> (# Int#, Int#, ByteArray# #)) -> Float# -> (Int# -> Int# -> ByteArray# -> r) -> r
composei3iii :: (Int# -> (# Int#, Int#, Int# #)) -> Int# -> (Int# -> Int# -> Int# -> r) -> r
composes3s__ :: (State# s -> (# State# t, a, b #)) -> State# s -> (State# t -> a -> b -> r) -> r
composes3sii :: (State# s -> (# State# t, Int#, Int# #)) -> State# s -> (State# t -> Int# -> Int# -> r) -> r
composes3si_ :: (State# s -> (# State# t, Int#, a #)) -> State# s -> (State# t -> Int# -> a -> r) -> r
composes3spw :: (State# s -> (# State# t, Addr#, Word# #)) -> State# s -> (State# t -> Addr# -> Word# -> r) -> r
composeb4ibib :: (ByteArray# -> (# Int#, ByteArray#, Int#, ByteArray# #)) -> ByteArray# -> (Int# -> ByteArray# -> Int# -> ByteArray# -> r) -> r
composed4iwwi :: (Double# -> (# Int#, Word#, Word#, Int# #)) -> Double# -> (Int# -> Word# -> Word# -> Int# -> r) -> r
composes4siii :: (State# s -> (# State# t, Int#, Int#, Int# #)) -> State# s -> (State# t -> Int# -> Int# -> Int# -> r) -> r

compose_1_ f x k = case f x of (# a #) -> k a
composei1_ f x k = case f x of (# a #) -> k a
composeo1_ f x k = case f x of (# a #) -> k a
composep1_ f x k = case f x of (# a #) -> k a
composew1_ f x k = case f x of (# a #) -> k a
compose_2ab f x k = case f x of (# a, b #) -> k a b
composeb2ib f x k = case f x of (# a, b #) -> k a b
composed2ii f x k = case f x of (# a, b #) -> k a b
composed2llii f x k = case f x of (# a, b #) -> k a b
composef2ii f x k = case f x of (# a, b #) -> k a b
composei2i_ f x k = case f x of (# a, b #) -> k a b
composei2ib f x k = case f x of (# a, b #) -> k a b
composei2ii f x k = case f x of (# a, b #) -> k a b
composeli2ib f x k = case f x of (# a, b #) -> k a b
composelli2ib f x k = case f x of (# a, b #) -> k a b
composellw2ib f x k = case f x of (# a, b #) -> k a b
composelw2ib f x k = case f x of (# a, b #) -> k a b
composep2ib f x k = case f x of (# a, b #) -> k a b
composes2s_ f x k = case f x of (# a, b #) -> k a b
composes2sa f x k = case f x of (# a, b #) -> k a b
composes2sb f x k = case f x of (# a, b #) -> k a b
composes2sc f x k = case f x of (# a, b #) -> k a b
composes2sd f x k = case f x of (# a, b #) -> k a b
composes2sf f x k = case f x of (# a, b #) -> k a b
composes2si f x k = case f x of (# a, b #) -> k a b
composes2sli f x k = case f x of (# a, b #) -> k a b
composes2slli f x k = case f x of (# a, b #) -> k a b
composes2sllw f x k = case f x of (# a, b #) -> k a b
composes2slw f x k = case f x of (# a, b #) -> k a b
composes2sMa f x k = case f x of (# a, b #) -> k a b
composes2sMb f x k = case f x of (# a, b #) -> k a b
composes2sMv f x k = case f x of (# a, b #) -> k a b
composes2so f x k = case f x of (# a, b #) -> k a b
composes2sp f x k = case f x of (# a, b #) -> k a b
composes2sSn f x k = case f x of (# a, b #) -> k a b
composes2sSp f x k = case f x of (# a, b #) -> k a b
composes2st f x k = case f x of (# a, b #) -> k a b
composes2sTv f x k = case f x of (# a, b #) -> k a b
composes2sv f x k = case f x of (# a, b #) -> k a b
composes2sw f x k = case f x of (# a, b #) -> k a b
composes2sWp f x k = case f x of (# a, b #) -> k a b
composew2ib f x k = case f x of (# a, b #) -> k a b
composew2wi f x k = case f x of (# a, b #) -> k a b
composew2ww f x k = case f x of (# a, b #) -> k a b
compose_3pab f x k = case f x of (# a, b, c #) -> k a b c
compose_3pba f x k = case f x of (# a, b, c #) -> k a b c
composed3iib f x k = case f x of (# a, b, c #) -> k a b c
composed3iii f x k = case f x of (# a, b, c #) -> k a b c
composef3iib f x k = case f x of (# a, b, c #) -> k a b c
composei3iii f x k = case f x of (# a, b, c #) -> k a b c
composes3s__ f x k = case f x of (# a, b, c #) -> k a b c
composes3sii f x k = case f x of (# a, b, c #) -> k a b c
composes3si_ f x k = case f x of (# a, b, c #) -> k a b c
composes3spw f x k = case f x of (# a, b, c #) -> k a b c
composeb4ibib f x k = case f x of (# a, b, c, d #) -> k a b c d
composed4iwwi f x k = case f x of (# a, b, c, d #) -> k a b c d
composes4siii f x k = case f x of (# a, b, c, d #) -> k a b c d

#if __GLASGOW_HASKELL__ >= 808
composehhi2hhihhi :: (Int8# -> (# Int8#, Int8# #)) -> Int8# -> (Int8# -> Int8# -> r) -> r
composehhw2hhwhhw :: (Word8# -> (# Word8#, Word8# #)) -> Word8# -> (Word8# -> Word8# -> r) -> r
composehi2hihi :: (Int16# -> (# Int16#, Int16# #)) -> Int16# -> (Int16# -> Int16# -> r) -> r
composehw2hwhw :: (Word16# -> (# Word16#, Word16# #)) -> Word16# -> (Word16# -> Word16# -> r) -> r
composehhi2hhihhi f x k = case f x of (# a, b #) -> k a b
composehhw2hhwhhw f x k = case f x of (# a, b #) -> k a b
composehi2hihi f x k = case f x of (# a, b #) -> k a b
composehw2hwhw f x k = case f x of (# a, b #) -> k a b
#endif
#if __GLASGOW_HASKELL >= 802
composes2sC :: (State# s -> (# State# t, Compact# #)) -> State# s -> (State# t -> Compact# -> r) -> r
composes3sCp :: (State# s -> (# State# t, Compact#, Addr# #)) -> State# s -> (State# t -> Compact# -> Addr# -> r) -> r
composes2sC f x k = case f x of (# a, b #) -> k a b
composes3sCp f x k = case f x of (# a, b, c #) -> k a b c
#endif
#if __GLASGOW_HASKELL__ >= 710
composes2sSa :: (State# s -> (# State# t, SmallArray# e #)) -> State# s -> (State# t -> SmallArray# e -> r) -> r
composes2sSMa :: (State# s -> (# State# t, SmallMutableArray# u e #)) -> State# s -> (State# t -> SmallMutableArray# u e -> r) -> r
composes2sSa f x k = case f x of (# a, b #) -> k a b
composes2sSMa f x k = case f x of (# a, b #) -> k a b
#endif
#if __GLASGOW_HASKELL__ >= 706
composes3s_2__ :: (State# s -> (# State# t, a, (# b, c #) #)) -> State# s -> (State# t -> a -> b -> c -> r) -> r
composes3s_2__ f x k = case f x of (# a, b, (# c, d #) #) -> k a b c d
#endif
#if __GLASGOW_HASKELL__ >= 704
composes2sAa :: (State# s -> (# State# t, ArrayArray# #)) -> State# s -> (State# t -> ArrayArray# -> r) -> r
composes2sMAa :: (State# s -> (# State# t, MutableArrayArray# u #)) -> State# s -> (State# t -> MutableArrayArray# u -> r) -> r
composes2sAa f x k = case f x of (# a, b #) -> k a b
composes2sMAa f x k = case f x of (# a, b #) -> k a b
#endif

-- | This is an \"inverse\" of 'composes2s_' because sometimes it might be
-- useful to produce a @'State#' s a -> (\# 'State#' s, a \#)@ of your own.
-- Example:
-- 
-- @returnIO x = 'IO' ('decomposes2s_' (\s r -> r s x))@
decomposes2s_ :: (State# s -> (State# t -> a -> (# State# t, a #)) -> (# State# t, a #)) -> State# s -> (# State# t, a #)
decomposes2s_ f x = f x (\a b -> (# a, b #))

decompose_2__ :: (a -> (b -> c -> (# b, c #)) -> (# b, c #)) -> a -> (# b, c #)
decompose_2__ f x = f x (\a b -> (# a, b #))
