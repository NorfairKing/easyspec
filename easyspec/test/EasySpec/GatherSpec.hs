module EasySpec.GatherSpec
    ( spec
    ) where

import TestImport
import TestUtils

import Language.Haskell.Exts
import Text.Show.Pretty

import EasySpec.Discover
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
                        unless (idType geid == idType seei) $
                        expectationFailure $
                        unlines
                            [ "The types that ghc and haskell-src-exts found differ for"
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
