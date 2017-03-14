{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module EasySpec.Discover.Gather where

import Import

import GHC
import GHC.Paths (libdir)
import OccName
import RdrName
import TcRnTypes

import EasySpec.OptParse

import EasySpec.Discover.Utils

getIds
    :: MonadIO m
    => DiscoverSettings -> m [GHC.Id]
getIds ds@DiscoverSettings {..} =
    liftIO $
    runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let compdflags = prepareFlags dflags
        setDFlagsNoLinking compdflags
        target <- guessTarget (toFilePath setDiscFile) Nothing
        setTargets [target]
        loadSuccessfully LoadAllTargets
                    -- Doesn't work in a project, only in top-level modules
        let modname = getTargetModName ds
        printO modname
        modSum <- getModSummary modname
        parsedModule <- parseModule modSum
        tmod <- typecheckModule parsedModule
        let (tcenv, _) = tm_internals_ tmod
        let names = concatMap (map gre_name) $ occEnvElts $ tcg_rdr_env tcenv
        fmap catMaybes $
            forM names $ \name -> do
                tything <- lookupName name
                pure $
                    case tything of
                        Just (AnId i) -> Just i
                        Just _ -> Nothing
                        Nothing -> Nothing
