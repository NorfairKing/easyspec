module EasySpecSpec
    ( spec
    ) where

import TestImport

import System.Environment (withArgs)

import EasySpec

spec :: Spec
spec =
    describe "examples" $ do
        exampleDir <- runIO $ resolveDir' "../examples"
        forSourceFilesInDir
            exampleDir
            (\f -> unwords ["works on", toFilePath f])
            (\f -> withArgs ["discover", toFilePath f] easyspec)

forSourceFilesInDir ::
       Example a
    => Path Abs Dir
    -> (Path Abs File -> String)
    -> (Path Abs File -> a)
    -> SpecWith (Arg a)
forSourceFilesInDir dir itfunc func = do
    fs <-
        runIO $ (filter ((== ".hs") . fileExtension) . snd) <$> listDirRecur dir
    forM_ fs $ \f -> it (itfunc f) $ func f
