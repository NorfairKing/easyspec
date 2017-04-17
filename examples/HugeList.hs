{-# LANGUAGE NoImplicitPrelude #-}

module HugeList where

import Data.List (sort, partition)
import Prelude
       (length, scanr, succ, snd, reverse, id, break, filter, scanl,
        zipWith, concat, zip, sum, (++), map, foldl, takeWhile, foldr,
        drop, dropWhile, span, unzip, (+), fst, take)

import Control.Monad ((>>=), (>=>))
