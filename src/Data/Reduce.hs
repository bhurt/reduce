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
--
-- = Introduction
--
-- This module provides the 'reduce' function.  It is similar to the
-- 'Data.Foldable.fold' function, in that it reduces a list of monoids
-- into a single monoid.  It is different in that it does so in a
-- balanced way- it only concatenates objects of similiar "size".
--
-- 
-- = Motivation
--
-- As a motivation for this library, consider the case where we have
-- implemented a simple tree data type:
--
-- @
--     data Tree a = Empty | Leaf a | Branch (Tree a) (Tree a)
--          deriving (Show, Read, Ord, Eq)
-- @
--
-- This datatype is a semigroup and a monoid in the obvious way (the
-- free monoid):
--
-- @
--      instance Semigroup (Tree a) where
--          Empty <> x     = x
--          x     <> Empty = x
--          x     <> y     = Branch x y
--
--      instance Monoid (Tree a) where
--          mempty = Empty
--          mappend = (<>)
-- @
--
-- We can now construct a tree with 'Data.Foldable.fold':
--
-- @
--      myTree :: Tree Int
--      myTree = fold (Leaf <$> [ 1 .. 8 ])
-- @
--
-- The problem with this is that the tree generated is wildly unbalanced:
--
-- @
-- Branch
--  (Leaf 1)
--  (Branch
--      (Leaf 2)
--      (Branch
--          (Leaf 3)
--          (Branch
--              (Leaf 4)
--              (Branch
--                  (Leaf 5)
--                  (Branch
--                      (Leaf 6)
--                      (Branch
--                          (Leaf 7)
--                          (Leaf 8)))))))
-- @
-- 
-- We've created a tree that looks like this:
--
-- ![An unbalanced tree](https://raw.githubusercontent.com/bhurt/reduce/main/docs/Diagram1.svg)
--
-- When what we want to create is a balanced tree, like:
-- ![A balanced tree](https://raw.githubusercontent.com/bhurt/reduce/main/docs/Diagram2.svg)
--
-- Replacing the call to 'Data.Foldable.fold' with a call to 'reduce'
-- does exactly that.  Given:
--
-- @
--      myTree :: Tree Int
--      myTree = reduce (Leaf <$> [ 1 .. 8 ])
-- @
--
-- The resulting tree produced is:
--
-- @
-- Branch
--      (Branch
--          (Branch
--              (Leaf 1)
--              (Leaf 2))
--          (Branch
--              (Leaf 3)
--              (Leaf 4)))
--      (Branch
--          (Branch
--              (Leaf 5)
--              (Leaf 6))
--          (Branch
--              (Leaf 7)
--              (Leaf 8)))
-- @
--
-- In other words, fold creates a list, while reduce creates a tree.
--
-- There is also performance implications.  Consider the case where
-- we are appending arrays, and need to copy the arrays every time
-- we call mappend.  In this case, a normal `Data.Foldable.fold` 
-- would be O(n^2).  However, a `reduce` would be O(n*log(n)),
-- potentially many orders of magnitude faster.
--
-- = To Be More Precise
--
-- The "size" of a value is the number of calls to 'mconcat' to
-- create that value.  Note that calling 'mconcat' with 'mempty'
-- as an argument still increases the size of that value by 1,
-- even though the value doesn't change (due to the monoid laws).
--
-- A call to mconcat is "balanced" if the size of either argument
-- is not larger than 2x+1 the size of the other.
--
-- If all calls to mconcat in a fold are balanced, then the fold
-- itself if balanced.
--
-- So the `Data.Foldable.fold` function is not a balanced fold,
-- because with an N-element container where N > 2, it will mconcat
-- a size 0 value (the first value) with a size N-1 value (the
-- concatation of the rest of the values).
--
-- = Implementation
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

    data Dig a = One a | Two a a

    incr :: (a -> a -> a) -> [ Dig a ] -> a -> [ Dig a ]
    incr _ []             a = [ One a ]
    incr _ (One a : xs)   b = Two a b : xs
    incr f (Two a b : xs) c = One c : incr f xs (f a b)


    fini :: forall a . (a -> a -> a) -> [ Dig a ] -> Maybe a
    fini _ []       = Nothing
    fini f (d : xs) = Just $ go xs (start d)
        where
            start :: Dig a -> a
            start (One a)   = a
            start (Two a b) = f a b

            go :: [ Dig a ] -> a -> a
            go []             a = a
            go (One a : ys)   b = go ys (f a b)
            go (Two a b : ys) c = go ys (f a (f b c))

    -- | Balanced reduce function on monoids
    --
    -- Similiar to 'Data.Foldable.fold'
    reduce :: forall a t . (Foldable t, Monoid a) => t a -> a
    reduce lst = 
        let s :: [ Dig a ]
            s = foldl (incr mappend) [] lst

            r :: Maybe a
            r = fini mappend s
        in
        case r of
            Just x  -> x
            Nothing -> mempty

    -- | Balanced reduce function with implicit map.
    --
    -- Similar to 'Data.Foldable.foldMap'
    reduceMap :: forall a t m . (Foldable t, Monoid m) => (a -> m) -> t a -> m
    reduceMap f lst =
        let g :: [ Dig m ] -> a -> [ Dig m ]
            g ds x = incr mappend ds (f x)

            s :: [ Dig m ]
            s = foldl g [] lst

            r :: Maybe m
            r = fini mappend s
        in
        case r of
            Just x  -> x
            Nothing -> mempty

    -- | Balanced reduce function with user-specified append function.
    --
    -- This function only returns 'Nothing' if the foldable is empty,
    -- otherwise it returns 'Just'.  This lets us avoid being a partial
    -- function.
    reduceWith :: forall a t . Foldable t => (a -> a -> a) -> t a -> Maybe a
    reduceWith f lst =
        let s :: [ Dig a ]
            s = foldl (incr f) [] lst
        in
        fini f s

