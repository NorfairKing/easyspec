{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

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
gatherSourceOf is IdData {..} = do
    mimpl <-
        case idDataExportingMods of
            (_:_) -> pure Nothing
        -- It was defined locally, so we may be able to get the implementation out of the current file.
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
                            (toEasyName $ Var.varName idDataId :: EasyName)
                            (() <$ mod_)
    case mimpl of
        Nothing -> pure ()
        Just impl ->
            debug1 $
            unwords
                [ "Found the following implementation for"
                , HSE.prettyPrint
                      (toEasyName $ Var.varName idDataId :: EasyName)
                , ":\n"
                , prettyEasyImpl impl
                ]
    pure mimpl

getImplFrom :: (Eq l, Monoid l) => Name l -> Module l -> Maybe (Impl l)
getImplFrom name mod_ =
    case mod_ of
        (Module _ _ _ _ decls) ->
            let mds =
                    flip map decls $ \d ->
                        let ifname n =
                                if name == n
                                    then Just d
                                    else Nothing
                        in case d of
                               FunBind _ (Match _ n _ _ _:_) -> ifname n
                               PatBind _ (PVar _ n) _ _ -> ifname n
                               PatSyn _ (PVar _ n) _ _ -> ifname n
                               _ -> Nothing
            in case catMaybes mds of
                   [] -> Nothing
                   ds -> Just $ Impl ds
        _ -> Nothing
