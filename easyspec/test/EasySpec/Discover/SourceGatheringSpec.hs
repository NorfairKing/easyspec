module EasySpec.Discover.SourceGatheringSpec
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
spec = do
    examplesDir <- runIO $ resolveDir' "../examples"
    forSourceFilesInDir
        examplesDir
        (\f ->
             unwords
                 [ "Easyspec found all the implementations of the functions defined in"
                 , toFilePath $ examplesDir </> f
                 ]) $ \f -> do
        ghcEasyIds <-
            runReaderT
                (getEasyIds
                     InputSpec
                     {inputSpecBaseDir = examplesDir, inputSpecFile = f})
                defaultSettings
        srcExtsEasyIds <- getHSEEasyIds examplesDir f
        forM_ srcExtsEasyIds $ \seei ->
            case find (\geid -> idName seei == idName seei) ghcEasyIds of
                Nothing ->
                    expectationFailure $
                    unwords
                        [ "haskell-src-exts found an id that GHC did not find:"
                        , prettyPrint $ idName seei
                        ]
                Just geid ->
                    unless (isJust $ idImpl geid) $
                    expectationFailure $
                    unlines
                        [ "haskell-src-exts found a function that easyspec did not find an implementation for:"
                        , unwords
                              [ prettyPrint $ idName seei
                              , "::"
                              , prettyPrint $ idType seei
                              ]
                        ]
