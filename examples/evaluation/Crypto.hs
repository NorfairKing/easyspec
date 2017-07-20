{-# LANGUAGE NoImplicitPrelude #-}

module Crypto where

import Prelude
       (Char, Int, (+), (-), (<), (==), (>), map, pred, succ)

shiftForwards :: Char -> Char
shiftForwards 'z' = 'a'
shiftForwards c = succ c

shiftBackwards :: Char -> Char
shiftBackwards 'a' = 'z'
shiftBackwards c = pred c

shiftStr :: Int -> [Char] -> [Char]
shiftStr num str
    | num > 0 = shiftStr (num - 1) (map shiftForwards str)
    | num < 0 = shiftStr (num + 1) (map shiftBackwards str)
    | num == 0 = str
