{-# LANGUAGE NoImplicitPrelude #-}

module Six where

import Prelude (Int, (++), (-), (<), id, map)

myDrop :: Int -> [a] -> [a]
myDrop x as =
    if x < 0
        then as
        else case as of
                 [] -> []
                 (a:rest) -> myDrop (x - 1) rest
