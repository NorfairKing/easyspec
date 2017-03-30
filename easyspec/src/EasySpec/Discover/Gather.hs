{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module EasySpec.Discover.Gather where

import Import

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

import EasySpec.Discover.Utils

getIds :: MonadIO m => DiscoverSettings -> m [GHC.Id]
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

data EasyId = EasyId
    { easyName :: EasyName
    , easyType :: EasyType
    } deriving (Show, Eq)

type EasyName = H.Name ()

type EasyType = H.Type ()

toEasyId :: GHC.Id -> EasyId
toEasyId i =
    EasyId
    { easyName = toEasyName $ Var.varName i
    , easyType = toEasyType $ Var.varType i
    }

toEasyName :: Monoid a => GHC.Name -> H.Name a
toEasyName n = Ident mempty $ showName n

toEasyType :: Monoid a => GHC.Type -> H.Type a
toEasyType ty =
    case splitFunTy_maybe ty of
        Nothing -> go ty
        Just (tf, tt) -> H.TyFun mempty (toEasyType tf) (toEasyType tt)
  where
    go t =
        case t of
            TyVarTy i -> TyVar mempty $ toEasyName $ Var.varName i
            AppTy t1 t2 -> TyApp mempty (toEasyType t1) (toEasyType t2)
            TyConApp tc kots ->
                let dres =
                        foldl
                            (TyApp mempty)
                            (TyCon mempty $
                             UnQual mempty $ toEasyName $ tyConName tc)
                            (map toEasyType kots)
                in case tyConClass_maybe tc of
                       Just _ -> dres
                       Nothing ->
                           case (showName (tyConName tc), kots) of
                               ("[]", [lt]) -> TyList mempty $ toEasyType lt
                               _ -> dres
            ForAllTy _ t' -> TyForall mempty Nothing Nothing $ toEasyType t'
            _ -> error "Not implemented yet"
