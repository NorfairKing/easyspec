{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Discover.SignatureGeneration where

import Import

import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax

import EasySpec.Discover.TH
import EasySpec.Discover.Types

{-# ANN module "HLint: ignore Use const" #-}

{-# ANN module "HLint: ignore Avoid lambda" #-}

{-# ANN module "HLint: ignore Collapse lambdas" #-}

createQuickspecSigExp :: [EasyId] -> Maybe EasyExp
createQuickspecSigExp ids = runQuickspecExp <$> createQuickspecSig ids

runQuickspecExp :: EasyExp -> EasyExp
runQuickspecExp = App () $(easyExp "QuickSpec.Eval.quickSpec")

showPrettyBackgroundExp :: EasyExp -> EasyExp
showPrettyBackgroundExp =
    App
        mempty
        $(easyExp $
          unwords
              [ "\\sig ->"
              , "map"
              , "(Text.PrettyPrint.HughesPJ.renderStyle"
              , "(Text.PrettyPrint.HughesPJ.style {Text.PrettyPrint.HughesPJ.mode = Text.PrettyPrint.HughesPJ.OneLineMode})"
              , "."
              , "Text.PrettyPrint.HughesPJClass.pPrint) $"
              , "map"
              , "(QuickSpec.Signature.prettyRename sig)"
              , "(QuickSpec.Signature.background sig)"
              ])

runQuickspecWithBackgroundExp :: Monoid m => Exp m -> Exp m -> Exp m
runQuickspecWithBackgroundExp bgsig =
    App
        mempty
        (App
             mempty
             (Var
                  mempty
                  (Qual
                       mempty
                       (ModuleName mempty "QuickSpec.Eval")
                       (Ident mempty "quickSpecWithBackground")))
             bgsig)

mappendSigsExp :: EasyExp -> EasyExp -> EasyExp
mappendSigsExp a =
    App
        mempty
        (App
             mempty
             (Var
                  mempty
                  (Qual
                       mempty
                       (ModuleName mempty "Data.Monoid")
                       (Ident mempty "mappend")))
             a)

createQuickspecSig :: (Eq m, Monoid m) => [Id m] -> Maybe (Exp m)
createQuickspecSig ids =
    (\es ->
         RecConstr
             mempty
             (Qual
                  mempty
                  (ModuleName mempty "QuickSpec.Signature")
                  (Ident mempty "signature"))
             [ FieldUpdate
                   mempty
                   (Qual
                        mempty
                        (ModuleName mempty "QuickSpec.Signature")
                        (Ident mempty "constants"))
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
                 (Var
                      mempty
                      (Qual
                           mempty
                           (ModuleName mempty "QuickSpec.Term")
                           (Ident mempty "constant")))
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
                                   (Qual
                                        mempty
                                        (ModuleName mempty "QuickSpec")
                                        (Ident mempty "Dict")))
                              (TyApp mempty (TyCon mempty cn) ct))
                         t')
            _ -> (e, t)

replaceVarsWithQuickspecVars :: Eq l => Type l -> Maybe (Type l)
replaceVarsWithQuickspecVars et =
    let tvs = getTyVars et
        funcs =
            map
                (\s ->
                     (\l ->
                          TyCon
                              l
                              (Qual l (ModuleName l "QuickSpec") (Ident l s))))
                ["A", "B", "C", "D", "E"]
    in replaceTyVars (zip tvs funcs) et

replaceTyVars :: Eq l => [(Name l, l -> Type l)] -> Type l -> Maybe (Type l)
replaceTyVars repls =
    foldType
        (\l mtvbs mctx -> fmap (TyForall l mtvbs mctx))
        (\l -> liftM2 (TyFun l))
        (\l b -> fmap (TyTuple l b) . sequence)
        (\l -> fmap (TyList l))
        (\l -> fmap (TyParArray l))
        (\l -> liftM2 (TyApp l))
        (\l n -> ($ l) <$> lookup n repls)
        (\l qn -> pure $ TyCon l qn)
        (\l -> fmap (TyParen l))
        (\l mt qn mv -> TyInfix l <$> mt <*> pure qn <*> mv)
        (\l mt k -> TyKind l <$> mt <*> pure k)
        (\l p -> pure $ TyPromoted l p)
        (\l mt1 mt2 -> TyEquals l <$> mt1 <*> mt2)
        (\l spl -> pure $ TySplice l spl)
        (\l bt up mt -> TyBang l bt up <$> mt)
        (\l mn -> pure $ TyWildCard l mn)
        (\l s1 s2 -> pure $ TyQuasiQuote l s1 s2)

getTyVars :: Type t -> [Name t]
getTyVars =
    foldType
        (\_ _ _ -> id)
        (\_ -> (++))
        (\_ _ -> concat)
        (\_ -> id)
        (\_ -> id)
        (\_ -> (++))
        (\_ -> (: []))
        (\_ _ -> [])
        (\_ -> id)
        (\_ v1 _ v2 -> v1 ++ v2)
        (\_ vs _ -> vs)
        (\_ _ -> [])
        (\_ -> (++))
        (\_ _ -> [])
        (\_ _ _ -> id)
        (\_ _ -> [])
        (\_ _ _ -> [])

foldType ::
       (l -> Maybe [TyVarBind l] -> Maybe (Context l) -> b -> b)
    -> (l -> b -> b -> b)
    -> (l -> Boxed -> [b] -> b)
    -> (l -> b -> b)
    -> (l -> b -> b)
    -> (l -> b -> b -> b)
    -> (l -> Name l -> b)
    -> (l -> QName l -> b)
    -> (l -> b -> b)
    -> (l -> b -> QName l -> b -> b)
    -> (l -> b -> Kind l -> b)
    -> (l -> Promoted l -> b)
    -> (l -> b -> b -> b)
    -> (l -> Splice l -> b)
    -> (l -> BangType l -> Unpackedness l -> b -> b)
    -> (l -> Maybe (Name l) -> b)
    -> (l -> String -> String -> b)
    -> Type l
    -> b
foldType ffa ff ft fl fpa fa fv fc fp fi fk fpr fe fspl fbng fwc fqq = go
  where
    go (TyForall l mtvbs btc t) = ffa l mtvbs btc (go t)
    go (TyFun l t1 t2) = ff l (go t1) (go t2)
    go (TyTuple l b ts) = ft l b (map go ts)
    go (TyList l lt) = fl l (go lt)
    go (TyParArray l lt) = fpa l (go lt)
    go (TyApp l t1 t2) = fa l (go t1) (go t2)
    go (TyVar l n) = fv l n
    go (TyCon l qn) = fc l qn
    go (TyParen l t) = fp l (go t)
    go (TyInfix l t1 qn t2) = fi l (go t1) qn (go t2)
    go (TyKind l t k) = fk l (go t) k
    go (TyPromoted l p) = fpr l p
    go (TyEquals l t1 t2) = fe l (go t1) (go t2)
    go (TySplice l spl) = fspl l spl
    go (TyBang l bt up t) = fbng l bt up (go t)
    go (TyWildCard l mn) = fwc l mn
    go (TyQuasiQuote l s1 s2) = fqq l s1 s2
