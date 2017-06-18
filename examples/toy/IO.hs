{-# LANGUAGE NoImplicitPrelude #-}

module IO where

import Prelude (IO, pure)

main :: IO ()
main = pure ()

twice :: IO () -> IO ()
twice func = do
    func
    func
