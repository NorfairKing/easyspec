{-# LANGUAGE FlexibleContexts #-}

module EasySpec.Discover.GatherFromGHC where

import Import

import ConLike
import DataCon
import GHC
import GHC.Paths (libdir)
import HscTypes
import OccName
import RdrName
import TcRnTypes

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
        getInstancesFromTcmodule tmod
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
                    Just (AConLike (RealDataCon dc)) -> [dataConWorkId dc]
                    Just _ -> []

getInstancesFromTcmodule :: GhcMonad m => TypecheckedModule -> m ()
getInstancesFromTcmodule tmod = do
    let (tcenv, md) = tm_internals_ tmod
    let insts = tcg_insts tcenv
    getInsts >>= printO
    printO insts
    printO $ md_insts md
    printO $ tcg_inst_env tcenv
    graph <- depanal [] True
    printO graph
    mods <-
        forM graph $ \mod_ -> do
            forM (ms_textual_imps mod_) $ \(_, imp) -> do
                let modname = unLoc imp
                pure modname
    ctx <- getContext
    setContext $ ctx ++ map (\mn -> IIDecl (simpleImportDecl mn)) (concat mods)
    loadSuccessfully LoadAllTargets
    getInsts >>= printO
    forM_ graph $ \mod_ -> do
        forM (ms_textual_imps mod_) $ \(_, imp) -> do
            let modname = unLoc imp
            mod_ <- lookupModule modname Nothing
            mmi <- getModuleInfo mod_
            case mmi of
                Nothing -> liftIO $ putStrLn "Nothing"
                Just mi -> printO $ mi_insts <$> modInfoIface mi
    forM_ graph $ \mod_ -> do
        forM_ (ms_textual_imps mod_) $ \(_, imp) -> do
            let modname = unLoc imp
            lookupModule modname Nothing >>= printO
            pure modname
            addTarget
                (Target
                 { targetId = TargetModule modname
                 , targetAllowObjCode = True
                 , targetContents = Nothing
                 })
            loadSuccessfully $ LoadUpTo modname
            getModSummary (unLoc imp) >>= printO
        tcmod <- parseModule mod_ >>= typecheckModule >>= loadModule
        let (tcenv', md') = tm_internals_ tcmod
        printO $ tcg_insts tcenv'
        printO $ md_insts md'
        printO $ tcg_inst_env tcenv'
    undefined
