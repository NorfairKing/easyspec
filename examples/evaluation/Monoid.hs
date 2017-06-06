{-# LANGUAGE NoImplicitPrelude #-}

module Monoid where

import Data.List (sort, nub)
import Prelude
       (Int, Num((+), (*), (-)), Ord((<=), (<), (>), (>=)), div, mod, odd,
        even, Bool(..), succ, pred, head, Eq((==), (/=)))

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
