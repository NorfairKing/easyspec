{-# LANGUAGE NoImplicitPrelude #-}

module Numerical.Triple where

import Prelude ((+), Num)

zero
    :: Num a
    => a
zero = 0

one
    :: Num a
    => a
one = 1

triple
    :: Num a
    => a -> a
triple x = x + x + x
