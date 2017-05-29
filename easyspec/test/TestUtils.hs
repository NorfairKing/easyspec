module TestUtils where

import TestImport

import Language.Haskell.Exts

import EasySpec.Discover.SourceGathering
import EasySpec.Discover.Types
import EasySpec.Utils

forSourceFilesInDir ::
       Example a
    => Path Abs Dir
    -> (Path Rel File -> String)
    -> (Path Rel File -> a)
    -> SpecWith (Arg a)
forSourceFilesInDir dir itfunc func = do
    fs <- runIO $ sourcesIn dir
    forM_ fs $ \f -> it (itfunc f) (func f)

getHSEEasyIds :: Path Abs Dir -> Path Rel File -> IO [EasyId]
getHSEEasyIds bd f = do
    pr <- parseFile $ toFilePath $ bd </> f
    case pr of
        ParseFailed srcloc err ->
            fail $
            unwords
                [ "haskell-src-exts failed to parse"
                , toFilePath f
                , "at"
                , show srcloc
                , "with error"
                , err
                ]
        ParseOk m -> pure $ getEasyIdsFrom $ () <$ m

getEasyIdsFrom :: Module () -> [EasyId]
getEasyIdsFrom m =
    case m of
        Module _ _ _ _ ds ->
            concat $
            flip map ds $ \d ->
                case d of
                    TypeSig _ ns t ->
                        map
                            (\n ->
                                 Id
                                 { idName = n
                                 , idType = t
                                 , idImpl = getImplFrom n m
                                 })
                            ns
                    _ -> []
        _ -> []
