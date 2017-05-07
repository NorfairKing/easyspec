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

import EasySpec.Discover.Types
import EasySpec.Discover.Utils

data IdData =
    IdData GHC.Id
           [GHC.ModuleName]

getGHCIds :: MonadIO m => InputSpec -> m [IdData]
getGHCIds is =
    liftIO $
    runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let compdflags =
                prepareFlags
                    dflags
                    { importPaths =
                          importPaths dflags ++
                          [toFilePath $ inputSpecBaseDir is]
                    }
        setDFlagsNoLinking compdflags
        let targetModName = getTargetModName $ inputSpecFile is
        target <- guessTarget (moduleNameString targetModName) Nothing
        setTargets [target]
        loadSuccessfully LoadAllTargets
            -- Doesn't work in a project, only in top-level modules
        modSum <- getModSummary targetModName
        parsedModule <- parseModule modSum
        tmod <- typecheckModule parsedModule
        getGHCIdsFromTcModule tmod

getGHCIdsFromTcModule :: GhcMonad m => TypecheckedModule -> m [IdData]
getGHCIdsFromTcModule tmod = do
    let (tcenv, _) = tm_internals_ tmod
        -- Get the global reader elementss out of the global env
    let gres = concat $ occEnvElts $ tcg_rdr_env tcenv
    fmap concat $
        forM gres $ \gre -> do
            tything <- lookupName $ gre_name gre
            let modulesThatImportThis = map (is_mod . is_decl) $ gre_imp gre
            pure $
                map (`IdData` modulesThatImportThis) $
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
