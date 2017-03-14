#!/usr/bin/env stack
{- stack
    --install-ghc
    runghc
    --package zifter-0.0.1.0
    --package zifter-cabal-0.0.0.0
    --package zifter-git-0.0.0.0
    --package zifter-hindent-0.0.0.0
    --package zifter-hlint-0.0.0.0
    --package zifter-stack-0.0.0.0
-}
{-# LANGUAGE OverloadedStrings #-}

import Zifter
import Zifter.Cabal
import Zifter.Git
import Zifter.Hindent
import Zifter.Hlint
import Zifter.Stack

main :: IO ()
main =
    ziftWith $ do
        recursiveZift
        preprocessor $ ziftP [hindentZift, cabalFormatZift]
        precheck gitAddAllZift
        checker $ do
            hlintZift
            stackBuildZift
