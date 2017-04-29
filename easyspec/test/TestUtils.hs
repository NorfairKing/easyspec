module TestUtils where

import TestImport

import EasySpec.Utils

forSourceFilesInDir ::
       Example a
    => Path Abs Dir
    -> (Path Rel File -> String)
    -> (Path Rel File -> a)
    -> SpecWith (Arg a)
forSourceFilesInDir dir itfunc func =
    void $ forSourcesIn dir $ \f -> it (itfunc f) $ func f
