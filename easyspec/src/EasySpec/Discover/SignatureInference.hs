module EasySpec.Discover.SignatureInference where

import Import

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Tree

import Language.Haskell.Exts.Syntax

import EasySpec.Discover.CodeUtils
import EasySpec.Discover.Types

{-# ANN module "HLint: ignore Avoid lambda" #-}

{-# ANN module "HLint: ignore Collapse lambdas" #-}

inferFullSignature :: SignatureInferenceStrategy
inferFullSignature =
    splitInferAlg "full-background" $ \focus scope -> scope \\ focus

inferEmptySignature :: SignatureInferenceStrategy
inferEmptySignature = splitInferAlg "empty-background" $ \_ _ -> []

infer5ByNameSyntactically :: SignatureInferenceStrategy
infer5ByNameSyntactically =
    splitInferAlg "5-by-name-syntactically" $ \focus scope ->
        take 5 $
        sortOn
            (\f -> sum $ map (\ff -> difference (dictOf ff) (dictOf f)) focus)
            scope
        -- TODO generalise to a bigger focus)
  where
    dictOf :: EasyId -> HashMap Char Int
    dictOf = letterDict . prettyPrintOneLine . idName

letterDict :: String -> HashMap Char Int
letterDict = foldl go HM.empty
  where
    go :: HashMap Char Int -> Char -> HashMap Char Int
    go hm k = HM.alter u k hm
      where
        u Nothing = Just 1
        u (Just n) = Just (n + 1)

difference :: HashMap Char Int -> HashMap Char Int -> Int
difference hm1 hm2 = HM.foldl' (+) 0 $ HM.unionWith go hm1 hm2
  where
    go :: Int -> Int -> Int
    go n1 n2 = abs (n1 - n2)

splitInferAlg
    :: String
    -> ([EasyId] -> [EasyId] -> [EasyId]) -- ^ Something that chooses the background ids.
    -> SignatureInferenceStrategy
splitInferAlg name func =
    SignatureInferenceStrategy name $ \focus scope ->
        let bgSigFuncs = func focus scope
            fgNExps = makeNamedExps focus
            bgNExps = makeNamedExps bgSigFuncs
        in InferredSignature $ Node fgNExps [Node bgNExps []]
  where
    makeNamedExps funcs = rights $ map convertToUsableNamedExp funcs

convertToUsableNamedExp :: EasyId -> Either String EasyNamedExp
convertToUsableNamedExp i = do
    let (e, t) = addTypeClassTrickery i
    t' <- replaceVarsWithQuickspecVars t
    pure
        NamedExp
        {neName = prettyPrintOneLine (idName i), neExp = ExpTypeSig mempty e t'}

addTypeClassTrickery :: EasyId -> (EasyExp, EasyType)
addTypeClassTrickery eid = go (expr, idType eid)
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

replaceVarsWithQuickspecVars
    :: Eq l
    => Type l -> Either String (Type l)
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
  where
    replaceTyVars
        :: Eq l
        => [(Name l, l -> Type l)] -> Type l -> Either String (Type l)
    replaceTyVars repls t =
        flip runReaderT False $
        -- Bool says 'whether it's higher-order'
        foldType
            (\l mtvbs mctx -> fmap (TyForall l mtvbs mctx))
            (\l -> liftM2 (TyFun l))
            (\l b -> fmap (TyTuple l b) . sequence)
            (\l -> fmap (TyList l))
            (\l -> fmap (TyParArray l))
            (\l et1 et2 -> do
                 t1 <- local (const True) et1
                 t2 <- et2
                 pure $ TyApp l t1 t2)
            (\l n -> do
                 b <- ask
                 lift $
                     if b
                         then Left
                                  "Higher-order type variables aren't supported yet."
                         else case ($ l) <$> lookup n repls of
                                  Nothing ->
                                      Left
                                          "Not enough type variables in QuickSpec"
                                  Just r -> pure r)
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
            t
