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

import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax hiding (ModuleName)

import EasySpec.Discover.Types

{-# ANN module "HLint: ignore Use const" #-}

{-# ANN module "HLint: ignore Avoid lambda" #-}

{-# ANN module "HLint: ignore Collapse lambdas" #-}

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
signatureComponent eid = do
    let (re, rt) = go (expr, idType eid)
    rt' <- replaceVarsWithQuickspecVars rt
    let funExp = ExpTypeSig mempty re rt'
    let funStr =
            prettyPrintStyleMode (style {mode = OneLineMode}) defaultMode $
            idName eid
    let funName = Lit mempty $ String mempty funStr funStr
    pure $
        App
            mempty
            (App
                 mempty
                 (Var mempty (UnQual mempty (Ident mempty "constant")))
                 funName)
            funExp
  where
    expr = Paren mempty $ Var mempty (UnQual mempty (idName eid))
    go (e, t) =
        case t of
            TyForall _ _ (Just (CxSingle _ (ClassA _ cn [ct]))) ft
            -- TODO support the other cases as wel
             ->
                let (e', t') = go (e, ft)
                in ( App
                         mempty
                         (Var mempty (UnQual mempty (Ident mempty "typeclass")))
                         e'
                   , TyFun
                         mempty
                         (TyApp
                              mempty
                              (TyCon
                                   mempty
                                   (UnQual mempty (Ident mempty "Dict")))
                              (TyApp mempty (TyCon mempty cn) ct))
                         t')
            _ -> (e, t)

replaceVarsWithQuickspecVars :: Eq l => Type l -> Maybe (Type l)
replaceVarsWithQuickspecVars et =
    let tvs = getTyVars et
        funcs =
            map (\s -> (\l -> TyVar l (Ident l s))) ["A", "B", "C", "D", "E"]
    in replaceTyVars (zip tvs funcs) et

replaceTyVars :: Eq l => [(Name l, l -> Type l)] -> Type l -> Maybe (Type l)
replaceTyVars repls =
    foldType
        (\l n -> ($l) <$> lookup n repls)
        (\l qn -> pure $ TyCon l qn)
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
    go (TyForall l mtvbs btc t) = ffa l mtvbs btc (go t)
    go (TyTuple l b ts) = ft l b (map go ts)
    go (TyList l lt) = fl l (go lt)
    go _ = error "not implemented yet"

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
