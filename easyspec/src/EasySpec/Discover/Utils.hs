{-# LANGUAGE FlexibleContexts #-}

module EasySpec.Discover.Utils where

import Import

import System.FilePath

import DynFlags hiding (Settings)
import GHC
import GHC.LanguageExtensions
import Name
import Outputable
import TyCoRep
import TyCon
import Type
import Var

import EasySpec.OptParse

setDFlagsNoLinking
    :: GhcMonad m
    => DynFlags -> m ()
setDFlagsNoLinking = void . setSessionDynFlags

loadSuccessfully
    :: GhcMonad m
    => LoadHowMuch -> m ()
loadSuccessfully hm = do
    r <- load hm
    case r of
        Succeeded -> pure ()
        Failed -> fail "Loading failed. No idea why."

prepareFlags :: DynFlags -> DynFlags
prepareFlags dflags = foldl xopt_set dflags [Cpp, ImplicitPrelude, MagicHash]

getTargetModName :: DiscoverSettings -> ModuleName
getTargetModName =
    mkModuleName . dropExtension . toFilePath . filename . setDiscFile

createQuickspecSig
    :: GhcMonad m
    => [GHC.Id] -> m String
createQuickspecSig ids = do
    componentExprs <- mapM idSigComponent ids
    let componentList = intercalate ", " componentExprs
    pure $ "signature { constants = [ " ++ componentList ++ " ] }"

idSigComponent
    :: GhcMonad m
    => GHC.Id -> m String
idSigComponent i = do
    name <- showGHC $ Var.varName i
    typs <- showGHC $ Var.varType i
    let typ = Var.varType i
    let tyS = typeStr typ
    liftIO $ print (name, tyS, typs)
    pure $ unwords ["constant", show name, "(" ++ name, "::", tyS ++ ")"]

typeStr :: GHC.Type -> String
typeStr t =
    case t of
        TyVarTy _ -> "A" -- TODO allow for other type variables too.
        AppTy t1 t2 ->
            let vn1 = typeStr t1
                vn2 = typeStr t2
            in unwords [vn1, vn2]
        TyConApp tc kots ->
            let cs = map typeStr kots
            in case showName (tyConName tc) of
                   "[]" -> "[" ++ unwords cs ++ "]"
                   tcn -> unwords $ tcn : cs
        ForAllTy _ t'
                -- No idea why this is necessary here...
         ->
            case splitFunTy_maybe t of
                Nothing -> typeStr t'
                Just (tf, tt) ->
                    let vn1 = typeStr tf
                        vn2 = typeStr tt
                    in unwords [vn1, "->", vn2]
        _ -> error "not implemented yet."

showName :: Name -> String
showName = occNameString . Name.nameOccName

showGHC
    :: (GhcMonad m, Outputable a)
    => a -> m String
showGHC a = do
    dfs <- getProgramDynFlags
    pure $ showPpr dfs a

printO
    :: (GhcMonad m, Outputable a)
    => a -> m ()
printO a = showGHC a >>= (liftIO . putStrLn)
