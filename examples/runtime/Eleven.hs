{-# LANGUAGE NoImplicitPrelude #-}

module Eleven where

import Prelude
       (Bool(True,False), (+), (-), concat, drop, map, take)

myId :: a -> a
myId a = a

myPlusPlus :: [a] -> [a] -> [a]
myPlusPlus (a:as) bs = a : myPlusPlus as bs
myPlusPlus [] bs = bs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (a:as) = as `myPlusPlus` [a]

