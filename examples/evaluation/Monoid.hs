{-# LANGUAGE NoImplicitPrelude #-}

module Monoid where

import Prelude (Int, (*), (+))

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
