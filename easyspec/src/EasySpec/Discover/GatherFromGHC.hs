{-# LANGUAGE FlexibleContexts #-}

module EasySpec.Discover.GatherFromGHC where

import Import

import DataCon
import GHC
import GHC.Paths (libdir)
import OccName
import RdrName
import TcRnTypes
import TyCon

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
        fmap concat $
            forM names $ \name -> do
                tything <- lookupName name
                pure $
                    case tything of
                        Nothing -> []
                        -- If it's a function, return it
                        Just (AnId i) -> [i]
                        -- If it's a data declaration, get its constructors as functions
                        Just (ATyCon tc) ->
                            if isVanillaAlgTyCon tc
                                then flip map (tyConDataCons tc) $ \dc ->
                                         dataConWorkId dc
                                else []
                        Just _ -> []
