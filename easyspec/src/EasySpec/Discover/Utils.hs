{-# LANGUAGE FlexibleContexts #-}

module EasySpec.Discover.Utils where

import Import

import System.FilePath

import DynFlags hiding (Settings)
import qualified GHC
import GHC
       (GhcMonad, SuccessFlag(..), LoadHowMuch, ModuleName,
        setSessionDynFlags, load, mkModuleName)
import GHC.LanguageExtensions
import qualified Name

import Language.Haskell.Exts.Pretty hiding (ModuleName)
import Language.Haskell.Exts.Syntax hiding (ModuleName)

import EasySpec.Discover.Types

setDFlagsNoLinking :: GhcMonad m => DynFlags -> m ()
setDFlagsNoLinking = void . setSessionDynFlags

loadSuccessfully :: GhcMonad m => LoadHowMuch -> m ()
loadSuccessfully hm = do
    r <- load hm
    case r of
        Succeeded -> pure ()
        Failed -> fail "Loading failed. No idea why."

prepareFlags :: DynFlags -> DynFlags
prepareFlags dflags = foldl xopt_set dflags [Cpp, ImplicitPrelude, MagicHash]

getTargetModName :: Path Abs File -> ModuleName
getTargetModName = mkModuleName . dropExtension . toFilePath . filename

createQuickspecSigExpStr :: [EasyId] -> Maybe String
createQuickspecSigExpStr ids =
    prettyPrintStyleMode (style {mode = OneLineMode}) defaultMode <$>
    createQuickspecSigExp ids

-- TODO make the names qualified
createQuickspecSigExp :: (Eq m, Monoid m) => [Id m] -> Maybe (Exp m)
createQuickspecSigExp ids =
    App mempty (Var mempty (UnQual mempty (Ident mempty "quickSpec"))) <$>
    createQuickspecSig ids

createQuickspecSig :: (Eq m, Monoid m) => [Id m] -> Maybe (Exp m)
createQuickspecSig ids =
    (\es ->
         RecConstr
             mempty
             (UnQual mempty (Ident mempty "signature"))
             [ FieldUpdate
                   mempty
                   (UnQual mempty (Ident mempty "constants"))
                   (List mempty es)
             ]) <$>
    mapM signatureComponent ids

signatureComponent :: (Eq m, Monoid m) => Id m -> Maybe (Exp m)
signatureComponent eid = ExpTypeSig mempty e <$> t
  where
    e = Var mempty (UnQual mempty (idName eid))
    t = replaceVarsWithQuickspecVars (idType eid)

--         unwords
--             [ "constant"
--             , show name
--             , "((" ++
--               (if "Dict" `isInfixOf` tyS
--                    then "typeclass (" ++ name ++ "))"
--                    else name ++ ")")
--             , "::"
--             , tyS ++ ")"
--             ]
replaceVarsWithQuickspecVars :: Eq l => Type l -> Maybe (Type l)
replaceVarsWithQuickspecVars et =
    let tvs = getTyVars et
        funcs =
            map (\s -> (\l -> TyVar l (Ident l s))) ["A", "B", "C", "D", "E"]
    in replaceTyVars (zip tvs funcs) et

replaceTyVars :: Eq l => [(Name l, l -> Type l)] -> Type l -> Maybe (Type l)
replaceTyVars repls =
    foldType
        (\l n -> fmap ($l) $ lookup n repls)
        (\l qn -> pure (TyCon l qn))
        (\l -> liftM2 (TyApp l))
        (\l -> liftM2 (TyFun l))
        (\l mtvbs mctx -> fmap (TyForall l mtvbs mctx))
        (\l b -> fmap (TyTuple l b) . sequence)
        (\l -> fmap (TyList l))

getTyVars :: Type t -> [Name t]
getTyVars =
    foldType
        (\_ -> (: []))
        (\_ _ -> [])
        (\_ -> (++))
        (\_ -> (++))
        (\_ _ _ -> id)
        (\_ _ -> concat)
        (\_ -> id)

foldType ::
       (l -> Name l -> b)
    -> (l -> QName l -> b)
    -> (l -> b -> b -> b)
    -> (l -> b -> b -> b)
    -> (l -> Maybe [TyVarBind l] -> Maybe (Context l) -> b -> b)
    -> (l -> Boxed -> [b] -> b)
    -> (l -> b -> b)
    -> Type l
    -> b
foldType fv fc fa ff ffa ft fl = go
  where
    go (TyVar l n) = fv l n
    go (TyCon l qn) = fc l qn
    go (TyApp l t1 t2) = fa l (go t1) (go t2)
    go (TyFun l t1 t2) = ff l (go t1) (go t2)
    go (TyForall l mtvbs btc ft) = ffa l mtvbs btc (go ft)
    go (TyTuple l b ts) = ft l b (map go ts)
    go (TyList l lt) = fl l (go lt)

-- typeStr :: [(GHC.Id, String)] -> GHC.Type -> String
-- typeStr env = go
--   where
--     go t =
--         case t of
--             TyVarTy i -> fromMaybe (showName $ Var.varName i) (lookup i env)
--             AppTy t1 t2 ->
--                 let vn1 = go t1
--                     vn2 = go t2
--                 in unwords [vn1, vn2]
--             TyConApp tc kots ->
--                 let cs = map go kots
--                     pars c = "(" ++ c ++ ")"
--                 in case tyConClass_maybe tc of
--                        Just cls ->
--                            concat
--                                [ "Dict ("
--                                , unwords $
--                                  showName (Class.className cls) : map pars cs
--                                , ")"
--                                ]
--                        Nothing ->
--                            case showName (tyConName tc) of
--                                "[]" -> "[" ++ unwords cs ++ "]"
--                                tcn -> unwords $ tcn : map pars cs
--             ForAllTy _ t'
--                     -- No idea why this is necessary here...
--              ->
--                 case splitFunTy_maybe t of
--                     Nothing -> go t'
--                     Just (tf, tt) ->
--                         let vn1 = go tf
--                             vn2 = go tt
--                         in unwords ["(" ++ vn1, "->", vn2 ++ ")"]
--             _ -> error "not implemented yet."
--
-- typeVars :: GHC.Type -> [GHC.Id]
-- typeVars t =
--     nub $
--     case t of
--         TyVarTy v -> [v]
--         AppTy t1 t2 -> typeVars t1 ++ typeVars t2
--         TyConApp _ kots -> concatMap typeVars kots
--         ForAllTy _ t' ->
--             case splitFunTy_maybe t of
--                 Nothing -> typeVars t'
--                 Just (tf, tt) -> typeVars tf ++ typeVars tt
--         _ -> error "not implemented yet."
showName :: GHC.Name -> String
showName = Name.occNameString . Name.nameOccName
--
-- showGHC :: (GhcMonad m, Outputable a) => a -> m String
-- showGHC a = do
--     dfs <- getProgramDynFlags
--     pure $ showPpr dfs a
--
-- printO :: (GhcMonad m, Outputable a) => a -> m ()
-- printO a = showGHC a >>= (liftIO . putStrLn)
