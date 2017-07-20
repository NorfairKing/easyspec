{-# LANGUAGE NoImplicitPrelude #-}

module Bools where

import Prelude
       (Bool(..), (&&),  (||), all, and, any, filter, map, not, or,
        zipWith)

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

impl :: Bool -> Bool -> Bool
impl b1 b2 = not b1 || b2

nand :: Bool -> Bool -> Bool
nand b1 b2 = not (b1 && b2)

nor :: Bool -> Bool -> Bool
nor b1 b2 = not (b1 || b2)

xnor :: Bool -> Bool -> Bool
xnor b1 b2 = not (xor b1 b2)
