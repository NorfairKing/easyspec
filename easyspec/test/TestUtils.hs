module TestUtils where

import TestImport

import Data.Char (isUpper)

import Language.Haskell.Exts

import EasySpec.Discover.SourceGathering
import EasySpec.Discover.Types
import EasySpec.Utils

forExamples ::
       Example a
    => (InputSpec -> String)
    -> (InputSpec -> a)
    -> SpecWith (Arg a)
forExamples f1 f2 = do
    exs <- runIO allExamples
    forM_ exs $ \ex -> it (f1 ex) (f2 ex)

forSourceFilesInDir ::
       Example a
    => Path Abs Dir
    -> (Path Rel File -> String)
    -> (Path Rel File -> a)
    -> SpecWith (Arg a)
forSourceFilesInDir dir itfunc func = do
    fs <- runIO $ sourcesIn dir
    forM_ fs $ \f -> it (itfunc f) (func f)

allExamples :: MonadIO m => m [InputSpec]
allExamples =
    liftIO $ do
        edir <- resolveDir' "../examples"
        walkDirAccum (Just descent) writer edir
  where
    descent ::
           MonadIO m
        => Path Abs Dir
        -> [Path Abs Dir]
        -> [Path Abs File]
        -> m WalkAction
    descent _ ds _ = do
        let startsWithUpper =
                (== Just True) . fmap isUpper . headMay . toFilePath . dirname
        pure $
            if all startsWithUpper ds
                then WalkFinish
                else WalkExclude $ filter startsWithUpper ds
    writer ::
           MonadIO m
        => Path Abs Dir
        -> [Path Abs Dir]
        -> [Path Abs File]
        -> m [InputSpec]
    writer bd _ fs =
        pure $
        mapMaybe
            (\f -> do
                 rf <- stripDir bd f
                 pure InputSpec {inputSpecFile = rf, inputSpecBaseDir = bd}) $
        filter isSourceFile fs

getHSEEasyIds :: InputSpec -> IO [EasyId]
getHSEEasyIds is = do
    pr <- parseFile $ toFilePath $ inputSpecFile is
    case pr of
        ParseFailed srcloc err ->
            fail $
            unwords
                [ "haskell-src-exts failed to parse"
                , toFilePath $ inputSpecFile is
                , "at"
                , show srcloc
                , "with error"
                , err
                ]
        ParseOk m -> pure $ getEasyIdsFrom $ () <$ m

getEasyIdsFrom :: Module () -> [EasyId]
getEasyIdsFrom m =
    case m of
        Module _ mmh _ _ ds ->
            concat $
            flip map ds $ \d ->
                case d of
                    TypeSig _ ns t ->
                        map
                            (\n ->
                                 Id
                                 { idName =
                                       case mmh of
                                           Nothing -> UnQual mempty n
                                           Just (ModuleHead _ mn _ _) ->
                                               Qual mempty mn n
                                 , idType = t
                                 , idImpl = getImplFrom (UnQual mempty n) m
                                 , idRootloc = Nothing
                                 })
                            ns
                    _ -> []
        _ -> []
