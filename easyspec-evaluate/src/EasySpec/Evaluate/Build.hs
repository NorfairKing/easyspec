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
    let groundFileName = $(mkRelFile "easyspec-evaluate.cabal")
    case find ((== groundFileName) . filename) fs of
        Nothing ->
            die $
            unlines
                [ "easyspec-evaluate build is being run in the wrong directory."
                , concat
                      [ "It must be run in the directory where you find "
                      , toFilePath groundFileName
                      , "."
                      ]
                ]
        Just _ ->
            withArgs ["--color", target] $
            shakeArgs
                shakeOptions
                { shakeVerbosity = Loud
                , shakeThreads = 0
                , shakeReport = ["report.trace", "report.html"]
                , shakeTimings = True
                }
                shakeBuild

shakeBuild :: Rules ()
shakeBuild = analyseRules
