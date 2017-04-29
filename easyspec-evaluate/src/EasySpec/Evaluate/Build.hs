module EasySpec.Evaluate.Build where

import Import

import Development.Shake

runBuild :: String -> IO ()
runBuild target = shakeArgs shakeOptions {shakeVerbosity = Loud} $ want [target]
