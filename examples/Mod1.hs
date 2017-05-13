{-# LANGUAGE NoImplicitPrelude #-}

module Mod1 where

import Prelude (Bool(..), not)

import Mod2

f :: Bool -> Int
f True = 1
f False = 0
