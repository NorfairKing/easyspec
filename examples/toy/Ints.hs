{-# LANGUAGE NoImplicitPrelude #-}

module Ints where

import Prelude (Int, (+), (-))

f :: Int -> Int
f x = x + 1

g :: Int -> Int
g x = x - 1

double :: Int -> Int
double x = x + x

zero :: Int
zero = 0
