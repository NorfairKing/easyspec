{-# LANGUAGE NoImplicitPrelude #-}

module Five where

import Prelude ((++), id, map, reverse)

myMConcat :: (a -> a -> a) -> a -> [a] -> a
myMConcat _ b [] = b
myMConcat func b (a:as) = func a (myMConcat func b as)
