module EasySpec.GatherSpec
    ( spec
    ) where

import TestImport
import TestUtils

import Language.Haskell.Exts
import Text.Show.Pretty

import EasySpec.Discover
import EasySpec.Discover.SourceGathering
import EasySpec.Discover.Types
import EasySpec.OptParse.Types

spec :: Spec
spec =
    describe "easyspec sources" $ do
        examplesDir <- runIO $ resolveDir' "../examples"
        forSourceFilesInDir
            examplesDir
            (\f ->
                 unwords
                     [ "The translation of what ghc sees in"
                     , toFilePath $ examplesDir </> f
                     , "matches what haskell-src-exts sees."
                     ]) $ \f -> do
            srcExtsEasyIds <- getHSEEasyIds examplesDir f
            ghcEasyIds <-
                runReaderT
                    (getEasyIds
                         InputSpec
                         {inputSpecBaseDir = examplesDir, inputSpecFile = f})
                    defaultSettings
            forM_ srcExtsEasyIds $ \seei ->
                case find (\geid -> idName seei == idName geid) ghcEasyIds of
                    Nothing ->
                        expectationFailure $
                        unwords
                            [ "haskell-src-exts found an id that GHC did not find:"
                            , prettyPrint $ idName seei
                            ]
                    Just geid ->
                        unless (geid == seei) $
                        expectationFailure $
                        unlines
                            [ "The that ghc and haskell-src-exts found differ for"
                            , prettyPrint $ idName seei
                            , ""
                            , "as a string:"
                            , unwords
                                  [ "ghcs translation:"
                                  , prettyPrint $ idType geid
                                  ]
                            , unwords
                                  [ "haskell-src-exts:"
                                  , prettyPrint $ idType seei
                                  ]
                            , ""
                            , "internally:"
                            , "ghc translation:"
                            , ppShow $ idType geid
                            , ""
                            , "haskell-src-exts:"
                            , ppShow $ idType seei
                            ]

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
