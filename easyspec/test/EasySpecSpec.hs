module EasySpecSpec
    ( spec
    ) where

import TestImport

import System.Environment (withArgs)

import EasySpec

spec :: Spec
spec =
    describe "examples" $
    it "works on Monomorphic.hs" $
    withArgs ["discover", "../examples/Monomorphic.hs"] easyspec
