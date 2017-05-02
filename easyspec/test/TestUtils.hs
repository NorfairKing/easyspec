module TestUtils where

import TestImport

import EasySpec.Utils

forSourceFilesInDir ::
       Example a
    => Path Abs Dir
    -> (Path Abs File -> String)
    -> (Path Abs File -> a)
    -> SpecWith (Arg a)
forSourceFilesInDir dir itfunc func = do
    fs <- runIO $ sourcesIn dir
    forM_ fs $ \f -> it (itfunc $ dir </> f) $ func (dir </> f)
