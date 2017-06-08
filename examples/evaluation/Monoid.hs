{-# LANGUAGE NoImplicitPrelude #-}

module Monoid where

import Data.Foldable (foldl')
import Data.List
       (sort, lookup, elem, splitAt, reverse, uncons, intersperse,
        intercalate, transpose, stripPrefix, group, inits, tails,
        isPrefixOf, isSuffixOf, isInfixOf, partition, nub, delete, (\\),
        union, intersect, sortOn, insert, isSubsequenceOf)
import Data.Maybe (isJust, isNothing)
import Prelude
       (Int, Num(..), Ord(..), div, mod, odd, even, Bool(..), Integer,
        Float, Double, Rational, Word, Monoid(..), id, const, flip, ($),
        take, drop, splitAt, takeWhile, dropWhile, span, break, lookup,
        zip, zip3, zipWith, zipWith3, unzip, unzip3, scanl, scanl1, scanr,
        scanr1, map, (++), filter, Bounded(..), Enum(..), words, lines,
        unlines, unwords, Either(..), either, Ordering(..), Char, fst, snd,
        curry, uncurry, Maybe(..), maybe, (&&), (||), not, head, Eq(..))

zero :: Int
zero = 0

add :: Int -> Int -> Int
add = (+)

negate :: Int -> Int
negate i = -i

one :: Int
one = 1

multiply :: Int -> Int -> Int
multiply = (*)
