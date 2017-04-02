{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module EasySpec.Discover.Gather where

import Import

import Class
import GHC
import GHC.Paths (libdir)
import OccName
import RdrName
import TcRnTypes
import TyCoRep
import TyCon
import Type
import Var

import Language.Haskell.Exts.Syntax as H

import EasySpec.OptParse

import EasySpec.Discover.Types as E
import EasySpec.Discover.Utils

getIds :: MonadIO m => Path Abs File -> m [EasyId]
getIds = fmap (map toEasyId) . getGHCIds

-- TODO use a Path Abs File instead of discoverSettings
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

toEasyId :: Monoid m => GHC.Id -> E.Id m
toEasyId i =
    Id
    {idName = toEasyName $ Var.varName i, idType = toEasyType $ Var.varType i}

toEasyName :: Monoid a => GHC.Name -> H.Name a
toEasyName n = Ident mempty $ showName n

toEasyType :: Monoid a => GHC.Type -> H.Type a
toEasyType ty =
    case splitFunTy_maybe ty of
        Just (tf, tt) ->
            case tf of
                TyConApp tc kots ->
                    case tyConClass_maybe tc of
                        Just c ->
                            TyForall
                                mempty
                                Nothing
                                (Just $
                                 CxSingle mempty $
                                 ClassA
                                     mempty
                                     (UnQual mempty $ toEasyName $ className c)
                                     (map toEasyType kots))
                                (toEasyType tt)
                        Nothing ->
                            H.TyFun mempty (toEasyType tf) (toEasyType tt)
                _ -> H.TyFun mempty (toEasyType tf) (toEasyType tt)
        Nothing ->
            case ty of
                TyVarTy i -> TyVar mempty $ toEasyName $ Var.varName i
                AppTy t1 t2 -> TyApp mempty (toEasyType t1) (toEasyType t2)
                TyConApp tc kots ->
                    case (showName (tyConName tc), kots) of
                        ("[]", [lt]) -> TyList mempty $ toEasyType lt
                        _ ->
                            foldl
                                (TyApp mempty)
                                (TyCon mempty $
                                 UnQual mempty $ toEasyName $ tyConName tc)
                                (map toEasyType kots)
                ForAllTy _ t' -> toEasyType t'
                _ -> error "Not implemented yet"
