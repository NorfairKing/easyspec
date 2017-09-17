module EasySpec.Discover.SourceGatheringSpec
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
    forExamples
        (\ex ->
             unwords
                 [ "Easyspec found all the implementations of the functions defined in"
                 , toFilePath $ inputSpecFile ex
                 ]) $ \ex -> do
        ghcEasyIds <- runReaderT (getEasyIds ex) defaultSettings
        -- the hse ids
        pr <- parseFile $ toFilePath $ inputSpecFile ex
        md <-
            case pr of
                ParseFailed srcloc err ->
                    fail $
                    unwords
                        [ "haskell-src-exts failed to parse"
                        , toFilePath $ inputSpecFile ex
                        , "at"
                        , show srcloc
                        , "with error"
                        , err
                        ]
                ParseOk m -> pure m
        let srcExtsEasyIds = getEasyIdsFrom $ () <$ md
        forM_ srcExtsEasyIds $ \seei ->
            case find (\geid -> idName seei == idName geid) ghcEasyIds of
                Nothing ->
                    expectationFailure $
                    unlines
                        [ "haskell-src-exts found an id that GHC did not find:"
                        , prettyPrint $ idName seei
                        , "haskell-src-exts found these ids:"
                        , unlines $ map prettyEasyId srcExtsEasyIds
                        , "ghc found these ids:"
                        , unlines $ map prettyEasyId ghcEasyIds
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
                        , "... with (found) implementation:"
                        , ppShow $ idImpl geid
                        , "... with name:"
                        , ppShow $ idName seei
                        , "in the following module:"
                        , ppShow $ () <$ md
                        ]
