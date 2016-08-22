{-# LANGUAGE UnboxedTuples, MagicHash #-}
{-# OPTIONS_GHC -fobject-code #-}
-------------------------------------------------------------------------------
-- |
-- Module      : Data.Tuple.Unboxed
-- Copyright   : (C) 2016 mniip
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
--  * d = 'Double#'
--  * f = 'Float#'
--  * i = 'Int#'
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
--  * Wp = 'Weak#'
--
-- This module contains functions for all combinations of types that come up in
-- "GHC.Prim" but in principle other cases could be added too.
-------------------------------------------------------------------------------
module Data.Tuple.Unboxed
    (
        composea1_,
        composei1_,
        composeo1_,
        composed2ii,
        composef2ii,
        composei2i_,
        composei2ii,
        composes2s_,
        composes2sa,
        composes2sAa,
        composes2sb,
        composes2sc,
        composes2sd,
        composes2sf,
        composes2si,
        composes2sSa,
        composes2sSMa,
        composes2sSn,
        composes2sSp,
        composes2sMa,
        composes2sMAa,
        composes2sMb,
        composes2sMv,
        composes2so,
        composes2sp,
        composes2st,
        composes2sTv,
        composes2sv,
        composes2sw,
        composes2sWp,
        composew2ww,
        compose_3pab,
        composes3si_,
        composed4iwwi,
        composes4siii,
        decomposes2s_,
    )
    where

import GHC.Prim

composea1_ :: (Addr# -> (# a #)) -> Addr# -> (a -> r) -> r
composei1_ :: (Int# -> (# a #)) -> Int# -> (a -> r) -> r
composeo1_ :: (BCO# -> (# a #)) -> BCO# -> (a -> r) -> r
composed2ii :: (Double# -> (# Int#, Int# #)) -> Double# -> (Int# -> Int# -> r) -> r
composef2ii :: (Float# -> (# Int#, Int# #)) -> Float# -> (Int# -> Int# -> r) -> r
composei2i_ :: (Int# -> (# Int#, a #)) -> Int# -> (Int# -> a -> r) -> r
composei2ii :: (Int# -> (# Int#, Int# #)) -> Int# -> (Int# -> Int# -> r) -> r
composes2s_ :: (State# s -> (# State# t, a #)) -> State# s -> (State# t -> a -> r) -> r
composes2sa :: (State# s -> (# State# t, Array# a #)) -> State# s -> (State# t -> Array# a -> r) -> r
composes2sAa :: (State# s -> (# State# t, ArrayArray# #)) -> State# s -> (State# t -> ArrayArray# -> r) -> r
composes2sb :: (State# s -> (# State# t, ByteArray# #)) -> State# s -> (State# t -> ByteArray# -> r) -> r
composes2sc :: (State# s -> (# State# t, Char# #)) -> State# s -> (State# t -> Char# -> r) -> r
composes2sd :: (State# s -> (# State# t, Double# #)) -> State# s -> (State# t -> Double# -> r) -> r
composes2sf :: (State# s -> (# State# t, Float# #)) -> State# s -> (State# t -> Float# -> r) -> r
composes2si :: (State# s -> (# State# t, Int# #)) -> State# s -> (State# t -> Int# -> r) -> r
composes2sSa :: (State# s -> (# State# t, SmallArray# a #)) -> State# s -> (State# t -> SmallArray# a -> r) -> r
composes2sSMa :: (State# s -> (# State# t, SmallMutableArray# u a #)) -> State# s -> (State# t -> SmallMutableArray# u a -> r) -> r
composes2sSn :: (State# s -> (# State# t, StableName# a #)) -> State# s -> (State# t -> StableName# a -> r) -> r
composes2sSp :: (State# s -> (# State# t, StablePtr# a #)) -> State# s -> (State# t -> StablePtr# a -> r) -> r
composes2sMa :: (State# s -> (# State# t, MutableArray# u a #)) -> State# s -> (State# t -> MutableArray# u a -> r) -> r
composes2sMAa :: (State# s -> (# State# t, MutableArrayArray# u #)) -> State# s -> (State# t -> MutableArrayArray# u -> r) -> r
composes2sMb :: (State# s -> (# State# t, MutableByteArray# u #)) -> State# s -> (State# t -> MutableByteArray# u -> r) -> r
composes2sMv :: (State# s -> (# State# t, MVar# u a #)) -> State# s -> (State# t -> MVar# u a -> r) -> r
composes2so :: (State# s -> (# State# t, BCO# #)) -> State# s -> (State# t -> BCO# -> r) -> r
composes2sp :: (State# s -> (# State# t, Addr# #)) -> State# s -> (State# t -> Addr# -> r) -> r
composes2st :: (State# s -> (# State# t, ThreadId# #)) -> State# s -> (State# t -> ThreadId# -> r) -> r
composes2sTv :: (State# s -> (# State# t, TVar# u a #)) -> State# s -> (State# t -> TVar# u a -> r) -> r
composes2sv :: (State# s -> (# State# t, MutVar# u a #)) -> State# s -> (State# t -> MutVar# u a -> r) -> r
composes2sw :: (State# s -> (# State# t, Word# #)) -> State# s -> (State# t -> Word# -> r) -> r
composes2sWp :: (State# s -> (# State# t, Weak# a #)) -> State# s -> (State# t -> Weak# a -> r) -> r
composew2ww :: (Word# -> (# Word#, Word# #)) -> Word# -> (Word# -> Word# -> r) -> r
compose_3pab :: (a -> (# Addr#, Array# b, ByteArray# #)) -> a -> (Addr# -> Array# b -> ByteArray# -> r) -> r
composes3si_ :: (State# s -> (# State# t, Int#, a #)) -> State# s -> (State# t -> Int# -> a -> r) -> r
composed4iwwi :: (Double# -> (# Int#, Word#, Word#, Int# #)) -> Double# -> (Int# -> Word# -> Word# -> Int# -> r) -> r
composes4siii :: (State# s -> (# State# t, Int#, Int#, Int# #)) -> State# s -> (State# t -> Int# -> Int# -> Int# -> r) -> r
composea1_ p x f = case p x of (# a #) -> f a
composei1_ p x f = case p x of (# a #) -> f a
composeo1_ p x f = case p x of (# a #) -> f a
composed2ii p x f = case p x of (# a, b #) -> f a b
composef2ii p x f = case p x of (# a, b #) -> f a b
composei2i_ p x f = case p x of (# a, b #) -> f a b
composei2ii p x f = case p x of (# a, b #) -> f a b
composes2s_ p x f = case p x of (# a, b #) -> f a b
composes2sa p x f = case p x of (# a, b #) -> f a b
composes2sAa p x f = case p x of (# a, b #) -> f a b
composes2sb p x f = case p x of (# a, b #) -> f a b
composes2sc p x f = case p x of (# a, b #) -> f a b
composes2sd p x f = case p x of (# a, b #) -> f a b
composes2sf p x f = case p x of (# a, b #) -> f a b
composes2si p x f = case p x of (# a, b #) -> f a b
composes2sSa p x f = case p x of (# a, b #) -> f a b
composes2sSMa p x f = case p x of (# a, b #) -> f a b
composes2sSn p x f = case p x of (# a, b #) -> f a b
composes2sSp p x f = case p x of (# a, b #) -> f a b
composes2sMa p x f = case p x of (# a, b #) -> f a b
composes2sMAa p x f = case p x of (# a, b #) -> f a b
composes2sMb p x f = case p x of (# a, b #) -> f a b
composes2sMv p x f = case p x of (# a, b #) -> f a b
composes2so p x f = case p x of (# a, b #) -> f a b
composes2sp p x f = case p x of (# a, b #) -> f a b
composes2st p x f = case p x of (# a, b #) -> f a b
composes2sTv p x f = case p x of (# a, b #) -> f a b
composes2sv p x f = case p x of (# a, b #) -> f a b
composes2sw p x f = case p x of (# a, b #) -> f a b
composes2sWp p x f = case p x of (# a, b #) -> f a b
composew2ww p x f = case p x of (# a, b #) -> f a b
compose_3pab p x f = case p x of (# a, b, c #) -> f a b c
composes3si_ p x f = case p x of (# a, b, c #) -> f a b c
composed4iwwi p x f = case p x of (# a, b, c, d #) -> f a b c d
composes4siii p x f = case p x of (# a, b, c, d #) -> f a b c d

-- | This is an \"inverse\" of 'composes2s_' because sometimes it might be
-- useful to produce a @'State#' s a -> (\# 'State#' s, a \#)@ of your own.
-- Example:
-- 
-- @returnIO x = 'IO' ('decomposes2s_' (\s r -> r s x))@
decomposes2s_ :: (State# s -> (State# s -> a -> (# State# s, a #)) -> (# State# s, a #)) -> State# s -> (# State# s, a #)
decomposes2s_ f x = f x (\a b -> (# a, b #))
