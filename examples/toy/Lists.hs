{-# LANGUAGE NoImplicitPrelude #-}

module Lists where

import Prelude (reverse, (++), map, head, last, tail, init)

cons :: a -> [a] -> [a]
cons a as = a : as
