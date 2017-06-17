{-# LANGUAGE NoImplicitPrelude #-}

module Monoid where

import Data.List
       ((\\), delete, elem, group, insert, intersperse, isInfixOf,
        isPrefixOf, isSubsequenceOf, isSuffixOf, lookup, nub, partition,
        reverse, sort, sortOn, splitAt, stripPrefix, uncons, union)
import Data.Maybe
       (Maybe(..), fromMaybe, isJust, isNothing, listToMaybe, mapMaybe,
        maybe, maybeToList)
import Prelude
       (Bool(..), Char, Double, Either(..), Eq(..), Float, Int, Integer,
        Monoid(..), Num(..), Ord((<), (<=), (>), (>=), max, min), Rational,
        Word, (&&), (++), (||), break, const, curry, div, drop, dropWhile,
        either, even, filter, flip, fst, id, lines, lookup, map, mod, not,
        odd, scanl, scanr, snd, span, splitAt, take, takeWhile, uncurry,
        unlines, unwords, unzip, words, zip, zipWith)

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
