{-# LANGUAGE NoImplicitPrelude #-}

module Four where

import Prelude ((++), id, reverse)

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (a:as) = f a : myMap f as
