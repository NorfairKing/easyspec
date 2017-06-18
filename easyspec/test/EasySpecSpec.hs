module EasySpecSpec
    ( spec
    ) where

import TestImport
import TestUtils

import System.Environment (withArgs)

import EasySpec
import EasySpec.Discover.Types

spec :: Spec
spec =
    describe "examples" $
    forExamples
        (\ex ->
             unwords
                 [ "works on"
                 , toFilePath $ inputSpecFile ex
                 , "without any focus functions, with the default signature inference strategy."
                 ])
        (\ex ->
             withArgs
                 [ "discover"
                 , toFilePath $ inputSpecFile ex
                 , "--base-dir"
                 , toFilePath $ inputSpecBaseDir ex
                 ]
                 easyspec)
