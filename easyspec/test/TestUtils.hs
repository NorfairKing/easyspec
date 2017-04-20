module TestUtils where

import TestImport

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
