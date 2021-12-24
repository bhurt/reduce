{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module       : Data.Reduce
-- Description  : Implementation of a balanced fold
-- Copyright    : (c) Brian Hurt, 2021
-- License      : BSD2
-- Maintainer   : bhurt42@gmail.com
-- Stability    : stable
-- Portability  : safe
--
-- This function is an obvious extension to Ralf Hinze's method for
-- [Constructing Red-Black Trees](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.46.2171).
-- You don't need to read that paper to use this module, but you do to
-- understand how it works.  Also, it's a really easy paper to read, and
-- really interesting and useful, so go read it anyways.
--

module Data.Reduce (
    reduce
    , reduceMap
    , reduceWith
) where

    reduce :: forall a t . (Foldable t, Monoid a) => t a -> a
    reduce = undefined

    reduceMap :: forall a t m . (Foldable t, Monoid m) => (a -> m) -> t a -> m
    reduceMap = undefined

    reduceWith :: forall a t . Foldable t => (a -> a -> a) -> t a -> a
    reduceWith = undefined
