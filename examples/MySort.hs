{-# LANGUAGE NoImplicitPrelude #-}

module MySort where

import Data.List (sort)
import Prelude (Bool(True), otherwise, (&&), Ord((<=)), Eq((==)))

mySort :: Ord a => [a] -> [a]
mySort [] = []
mySort (x:xs) = insert (mySort xs)
  where
    insert [] = [x]
    insert (y:ys)
        | x <= y = x : y : ys
        | otherwise = y : insert ys

myIsSorted :: Ord a => [a] -> Bool
myIsSorted [] = True
myIsSorted [_] = True
myIsSorted (x:y:ls) = x <= y && myIsSorted (y : ls)

myIsPermutation :: Ord a => [a] -> [a] -> Bool
myIsPermutation a b = sort a == sort b
