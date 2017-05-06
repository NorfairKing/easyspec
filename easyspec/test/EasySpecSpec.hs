module EasySpecSpec
    ( spec
    ) where

import TestImport

import System.Environment (withArgs)

import EasySpec
import TestUtils

spec :: Spec
spec =
    describe "examples" $ do
        exampleDir <- runIO $ resolveDir' "../examples"
        forSourceFilesInDir
            exampleDir
            (\f -> unwords ["works on", toFilePath f])
            (\f ->
                 withArgs
                     ["discover", toFilePath f, "--base-dir", toFilePath exampleDir]
                     easyspec)
