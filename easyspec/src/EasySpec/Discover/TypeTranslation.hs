{-# LANGUAGE FlexibleContexts #-}

module EasySpec.Discover.TypeTranslation where

import Import hiding (tyConName)

import Class
import GHC
import qualified Module
import qualified Name
import TyCoRep
import TyCon
import Type
import Var

import Language.Haskell.Exts.Syntax as H

import EasySpec.Discover.GatherFromGHC as E
import EasySpec.Discover.Types as E

toEasyId :: Monoid m => E.IdData -> Maybe (E.Impl m) -> E.Id m
toEasyId d impl =
    Id
    { E.idName = toEasyQName d
    , E.idType = toEasyType $ Var.varType $ idDataId d
    , E.idImpl = impl
    , E.idRootloc = idDataRootloc d
    }

toEasyName :: Monoid a => GHC.Name -> H.Name a
toEasyName n = Ident mempty $ showName n

toEasyQName :: Monoid a => IdData -> H.QName a
toEasyQName d =
    let ident = toEasyName $ Var.varName $ idDataId d
    in case idDataExportingMods d of
           [] -> H.UnQual mempty ident -- FIXME this will break things, most likely
           (mn:_) ->
               H.Qual
                   mempty
                   (ModuleName mempty $ Module.moduleNameString mn)
                   ident

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
                                     (UnQual mempty $ toEasyName $ className c) -- FIXME use a qualified name
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
                        ("()", []) ->
                            TyCon mempty $ Special mempty $ UnitCon mempty
                        _ ->
                            foldl
                                (TyApp mempty)
                                (TyCon mempty $
                                 UnQual mempty $ toEasyName $ tyConName tc)
                                (map toEasyType kots)
                ForAllTy _ t' -> toEasyType t'
                _ -> error "Not implemented yet"

showName :: GHC.Name -> String
showName = Name.occNameString . Name.nameOccName
