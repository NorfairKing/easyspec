{-# LANGUAGE NoImplicitPrelude #-}

module MySort where

import Prelude (Bool(True), otherwise, (&&), Ord((<=)))

mySort
    :: Ord a
    => [a] -> [a]
mySort [] = []
mySort (x:xs) = insert (mySort xs)
  where
    insert [] = [x]
    insert (y:ys)
        | x <= y = x : y : ys
        | otherwise = y : insert ys

myIsSorted
    :: Ord a
    => [a] -> Bool
myIsSorted [] = True
myIsSorted [_] = True
myIsSorted (x:y:ls) = x <= y && myIsSorted (y : ls)
