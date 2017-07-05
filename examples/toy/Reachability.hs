{-# LANGUAGE NoImplicitPrelude #-}

module Reachability where

import Data.Char (chr)
import Prelude
       (Bool(True), Int, Ordering(), String, (+), id, length)

f :: Ordering -> ()
f _ = ()

g :: () -> Bool
g () = True

i :: Int -> Int
i = (+ 1)

j :: Int -> Int
j = (+ 2)

k :: Int -> Int
k = (+ 3)

l :: Int -> Int
l = (+ 4)

m :: Int -> Int
m = (+ 5)

n :: Int -> Int
n = (+ 6)

o :: Int -> Int
o = (+ 7)

p :: Int -> Int
p = (+ 8)

q :: Int -> Int
q = (+ 9)

r :: Int -> Int
r = (+ 10)
