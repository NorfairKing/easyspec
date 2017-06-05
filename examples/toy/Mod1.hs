{-# LANGUAGE NoImplicitPrelude #-}

module Mod1 where

import Prelude (Bool(..), not, Int)

import Mod1.Mod2

f :: Bool -> Int
f True = 1
f False = 0
