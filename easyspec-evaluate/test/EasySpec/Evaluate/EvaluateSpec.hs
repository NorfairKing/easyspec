module EasySpec.Evaluate.EvaluateSpec
    ( spec
    ) where

import TestImport

import EasySpec.Evaluate.Build

spec :: Spec
spec = describe "easyspec-evaluate" $ it "builds everything" runBuildEverything
