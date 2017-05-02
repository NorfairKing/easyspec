{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EasySpec.Evaluate.Build where

import Import

import System.Environment
import System.Exit

import EasySpec.Evaluate.Analyse

import Development.Shake

runBuild :: String -> IO ()
runBuild target = do
    here <- getCurrentDir
    fs <- snd <$> listDir here
    case find ((== $(mkRelFile "easyspec-evaluate.cabal")) . filename) fs of
        Nothing ->
            die "easyspec-evaluate build is being run in the wrong directory."
        Just _ ->
            withArgs ["--color", target] $
            shakeArgs
                shakeOptions {shakeVerbosity = Loud, shakeThreads = 0}
                shakeBuild

shakeBuild :: Rules ()
shakeBuild = analyseRules
