module EasySpec.EvaluateSpec
    ( spec
    ) where

import TestImport

import System.Environment (withArgs)

import EasySpec.Evaluate

spec :: Spec
spec =
    describe "easyspec-evaluate" $
    it "runs" $
    withArgs ["evaluate", "--examples-dir", "../examples"] easyspecEvaluate
