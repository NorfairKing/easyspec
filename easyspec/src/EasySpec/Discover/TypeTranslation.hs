{-# LANGUAGE FlexibleContexts #-}

module EasySpec.Discover.TypeTranslation where

import Import hiding (tyConName)

import System.FilePath as FP

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
    { E.idName = toEasyQNameFromSources d
    , E.idType = toEasyType $ Var.varType $ idDataId d
    , E.idImpl = impl
    , E.idRootloc = idDataRootloc d
    }

toEasyName :: Monoid a => GHC.Name -> H.Name a
toEasyName n = Ident mempty $ showName n

toEasyQName :: Monoid a => GHC.Name -> String -> H.QName a
toEasyQName name modname =
    H.Qual mempty (ModuleName mempty modname) (toEasyName name)

toEasyQNameFromSources :: Monoid a => IdData -> H.QName a
toEasyQNameFromSources d =
    case idDataExportingMods d of
        [] ->
            case idDataRootloc d of
                Nothing ->
                    UnQual mempty (toEasyName $ Var.varName $ idDataId d) -- This should not occur, but it's not enforced by the type system.
                Just fp ->
                    toEasyQName (Var.varName $ idDataId d) $
                    map
                        (\c ->
                             case c of
                                 '/' -> '.'
                                 _ -> c) $
                    FP.dropExtensions $ toFilePath fp
        (mn:_) ->
            toEasyQName (Var.varName $ idDataId d) (Module.moduleNameString mn)

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
                        ("(,)", [t1, t2]) ->
                            TyTuple mempty Boxed [toEasyType t1, toEasyType t2]
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
