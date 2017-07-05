{-# LANGUAGE NoImplicitPrelude #-}

module Lists where

import Prelude ((++), head, init, last, map, reverse, tail)

cons :: a -> [a] -> [a]
cons a as = a : as
