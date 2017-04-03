module EasySpec.GatherSpec
    ( spec
    ) where

import TestImport
import TestUtils

import Language.Haskell.Exts
import Text.Show.Pretty

import EasySpec.Discover.GatherFromGHC
import EasySpec.Discover.TypeTranslation
import EasySpec.Discover.Types

spec :: Spec
spec =
    describe "easyspec sources" $ do
        easyspecSourceDir <- runIO $ resolveDir' "../examples"
        forSourceFilesInDir
            easyspecSourceDir
            (\f ->
                 unwords
                     [ toFilePath f
                     , "matches what ghc sees with what haskell-src-exts sees"
                     ]) $ \f -> do
            srcExtsEasyIds <- getHSEEasyIds f
            ghcEasyIds <- map toEasyId <$> getGHCIds f
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

getHSEEasyIds :: Path Abs File -> IO [EasyId]
getHSEEasyIds f = do
    pr <- parseFile $ toFilePath f
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
        ParseOk m -> pure $ getEasyIdsFrom m

getEasyIdsFrom :: Module a -> [EasyId]
getEasyIdsFrom m =
    case () <$ m of
        Module _ _ _ _ ds ->
            concat $
            flip map ds $ \d ->
                case d of
                    TypeSig _ ns t -> map (\n -> Id {idName = n, idType = t}) ns
                    _ -> []
        _ -> []
