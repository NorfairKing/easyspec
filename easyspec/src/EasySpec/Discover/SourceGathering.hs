{-# LANGUAGE FlexibleContexts #-}

module EasySpec.Discover.SourceGathering where

import Import

import Var

import Language.Haskell.Exts.Parser as HSE
import Language.Haskell.Exts.Pretty as HSE
import Language.Haskell.Exts.Syntax as HSE

import EasySpec.Discover.GatherFromGHC
import EasySpec.Discover.TypeTranslation
import EasySpec.Discover.Types
import EasySpec.OptParse.Types
import EasySpec.Utils

gatherSourceOf ::
       (MonadIO m, MonadReader Settings m)
    => InputSpec
    -> IdData
    -> m (Maybe EasyImpl)
gatherSourceOf is (IdData i ms) = do
    mimpl <-
        case ms of
            (_:_) -> pure Nothing
        -- It was defined locally, so we can get the implementation out of the current file.
            [] -> do
                let sourceFile = inputSpecAbsFile is
                mainContents <- liftIO $ readFile $ toFilePath sourceFile
                case parseModule mainContents of
                    ParseFailed loc err -> do
                        debug1 $
                            unwords
                                [ "Unable to get source from"
                                , toFilePath sourceFile
                                , "because of error"
                                , show err
                                , "at"
                                , show loc
                                ]
                        pure Nothing
                    ParseOk mod_ ->
                        pure $
                        getImplFrom
                            (toEasyName $ Var.varName i :: EasyName)
                            (() <$ mod_)
    case mimpl of
        Nothing -> pure ()
        Just impl ->
            debug1 $
            unwords
                [ "Found the following implementation for"
                , HSE.prettyPrint (toEasyName $ Var.varName i :: EasyName)
                , ":\n"
                , prettyEasyImpl impl
                ]
    pure mimpl

getImplFrom :: (Eq l, Monoid l) => Name l -> Module l -> Maybe (Impl l)
getImplFrom name mod_ =
    case mod_ of
        (Module _ _ _ _ decls) ->
            let ds =
                    flip map decls $ \d ->
                        case d of
                            FunBind _ [] -> Nothing
                            FunBind _ mtcs@(Match _ n _ _ _:_) ->
                                if name == n
                                    then Just mtcs
                                    else Nothing
                            _ -> Nothing
            in case catMaybes ds of
                   (h:_) -> Just $ Impl h
                   _ -> Nothing
        _ -> Nothing
