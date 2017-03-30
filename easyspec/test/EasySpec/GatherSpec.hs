module EasySpec.GatherSpec
    ( spec
    ) where

import TestImport
import TestUtils

import Language.Haskell.Exts
import Text.Show.Pretty

import EasySpec.Discover.Gather
import EasySpec.OptParse.Types

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
            ghcEasyIds <- getGHCEasyIds f
            forM_ srcExtsEasyIds $ \seei ->
                case find (\geid -> easyName seei == easyName geid) ghcEasyIds of
                    Nothing ->
                        expectationFailure $
                        unwords
                            [ "haskell-src-exts found an id that GHC did not find:"
                            , prettyPrint $ easyName seei
                            ]
                    Just geid ->
                        unless (geid == seei) $
                        expectationFailure $
                        unlines
                            [ "The that ghc and haskell-src-exts found differ for"
                            , prettyPrint $ easyName seei
                            , ""
                            , "as a string:"
                            , unwords
                                  [ "ghcs translation:"
                                  , prettyPrint $ easyType geid
                                  ]
                            , unwords
                                  [ "haskell-src-exts:"
                                  , prettyPrint $ easyType seei
                                  ]
                            , ""
                            , "internally:"
                            , "ghc translation:"
                            , ppShow $ easyType geid
                            , ""
                            , "haskell-src-exts:"
                            , ppShow $ easyType seei
                            ]

getGHCEasyIds :: Path Abs File -> IO [EasyId]
getGHCEasyIds f = do
    ghcIds <- getIds DiscoverSettings {setDiscFile = f, setDiscFun = Nothing}
    pure $ map toEasyId ghcIds

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
                    TypeSig _ ns t ->
                        map (\n -> EasyId {easyName = n, easyType = t}) ns
                    _ -> []
        _ -> []
