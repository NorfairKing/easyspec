{-# LANGUAGE FlexibleContexts #-}

module EasySpec.Discover.TypeTranslation where

import Import hiding (tyConName)

import Class
import GHC
import qualified Name
import TyCoRep
import TyCon
import Type
import Var

import Language.Haskell.Exts.Syntax as H

import EasySpec.Discover.Types as E

toEasyId ::
       Monoid m => GHC.Id -> Maybe (E.Impl m) -> Maybe (Path Rel File) -> E.Id m
toEasyId i impl mrl =
    Id
    { E.idName = toEasyName $ Var.varName i
    , E.idType = toEasyType $ Var.varType i
    , E.idImpl = impl
    , E.idRootloc = mrl
    }

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
