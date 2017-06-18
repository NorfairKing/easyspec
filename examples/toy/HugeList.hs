{-# LANGUAGE NoImplicitPrelude #-}

module HugeList where

import Data.List (partition, sort)
import Prelude
       ((+), (++), break, concat, drop, dropWhile, filter, foldl, foldr,
        fst, id, length, map, reverse, scanl, scanr, snd, span, succ, sum,
        take, takeWhile, unzip, zip, zipWith)

import Control.Monad ((>=>), (>>=))
