{-# LANGUAGE FlexibleContexts #-}

module EasySpec.Discover.GatherFromGHC where

import Import

import ConLike
import DataCon
import GHC
import GHC.Paths (libdir)
import Name
import OccName
import RdrName
import TcRnTypes
import Var

import EasySpec.Discover.Types
import EasySpec.Discover.Utils

data IdData = IdData
    { idDataId :: GHC.Id
    , idDataExportingMods :: [GHC.ModuleName]
    , idDataRootloc :: Maybe (Path Rel File)
    } deriving (Eq)

getGHCIds
    :: MonadIO m
    => InputSpec -> m [IdData]
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
        modSum <- getModSummary targetModName
        parsedModule <- parseModule modSum
        tmod <- typecheckModule parsedModule
        getGHCIdsFromTcModule (inputSpecFile is) tmod

getGHCIdsFromTcModule
    :: GhcMonad m
    => Path Rel File -> TypecheckedModule -> m [IdData]
getGHCIdsFromTcModule file tmod = do
    let (tcenv, _) = tm_internals_ tmod
        -- Get the global reader elementss out of the global env
    let gres = concat $ occEnvElts $ tcg_rdr_env tcenv
    let isInternal =
            (\n -> "$tr" `isPrefixOf` n || "$tc" `isPrefixOf` n) .
            Name.getOccString . Var.varName . idDataId
    let locals =
            filter (not . isInternal) $
            concat $
            flip map (modInfoTyThings $ tm_checked_module_info tmod) $ \tything ->
                flip map (idsFromThing tything) $ \i ->
                    IdData
                    { idDataId = i
                    , idDataExportingMods = []
                    , idDataRootloc = Just file
                    }
    others <-
        fmap concat $
        forM gres $ \gre -> do
            mtything <- lookupName $ gre_name gre
            let modulesFromWhichWeImportedThis =
                    map (is_mod . is_decl) $ gre_imp gre
            pure $
                case mtything of
                    Nothing -> []
                    Just tything ->
                        flip map (idsFromThing tything) $ \i ->
                            IdData
                            { idDataId = i
                            , idDataExportingMods =
                                  modulesFromWhichWeImportedThis
                            , idDataRootloc = Nothing
                            }
    pure $ nubBy (\i1 i2 -> idDataId i1 == idDataId i2) $ locals ++ others
  where
    idsFromThing :: GHC.TyThing -> [GHC.Id]
    idsFromThing tything =
        case tything of
            AnId i -> [i]
                   -- If it's a function, return it
            AConLike (RealDataCon dc) -> [dataConWorkId dc]
                   -- If it's a data declaration, get its constructors as functions
            _ -> []
