{-# LANGUAGE FlexibleContexts #-}

module EasySpec.Discover.GatherFromGHC where

import Import

import GHC
import GHC.Paths (libdir)
import OccName
import RdrName
import TcRnTypes

import EasySpec.Discover.Utils

getGHCIds :: MonadIO m => Path Abs File -> m [GHC.Id]
getGHCIds discFile =
    liftIO $
    runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let compdflags = prepareFlags dflags
        setDFlagsNoLinking compdflags
        target <- guessTarget (toFilePath discFile) Nothing
        setTargets [target]
        loadSuccessfully LoadAllTargets
        -- Doesn't work in a project, only in top-level modules
        let modname = getTargetModName discFile
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
