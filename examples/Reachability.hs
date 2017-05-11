{-# LANGUAGE NoImplicitPrelude #-}

module Reachability where

import Data.Char (chr)
import Prelude (Char, Int, length, Double, String, id, (+))

f :: Int -> Char
f = chr

g :: Char -> ()
g _ = ()

h :: String -> Int
h = length

i :: Double -> Double
i = (+ 1)

j :: Double -> Double
j = (+ 2)

k :: Double -> Double
k = (+ 3)

l :: Double -> Double
l = (+ 4)

m :: Double -> Double
m = (+ 5)

n :: Double -> Double
n = (+ 6)
