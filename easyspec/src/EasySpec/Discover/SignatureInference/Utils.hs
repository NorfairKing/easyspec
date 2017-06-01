{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Discover.SignatureInference.Utils where

import Import

import Data.Tree

import Language.Haskell.Exts.Syntax

import EasySpec.Discover.CodeUtils
import EasySpec.Discover.Types

{-# ANN module "HLint: ignore Avoid lambda" #-}

{-# ANN module "HLint: ignore Collapse lambdas" #-}

unorderedCombinations :: [a] -> [(a, a)]
unorderedCombinations ls =
    concatMap
        (\t ->
             case t of
                 [] -> []
                 (l1:l2s) -> map ((,) l1) l2s) $
    tails ls

unionInferAlg ::
       SignatureInferenceStrategy
    -> SignatureInferenceStrategy
    -> SignatureInferenceStrategy
unionInferAlg si1 si2 =
    SignatureInferenceStrategy
    { sigInfStratName =
          intercalate
              "-"
              ["union", "of", sigInfStratName si1, "and", sigInfStratName si2]
    , sigInfRelevantSources =
          $(mkRelFile __FILE__) :
          (sigInfRelevantSources si1 ++ sigInfRelevantSources si2)
    , inferSignature =
          \ei1 ei2 ->
              let InferredSignature s1 = inferSignature si1 ei1 ei2
                  InferredSignature s2 = inferSignature si2 ei1 ei2
              in InferredSignature $ s1 ++ s2
    }

splitInferAlg ::
       String
    -> [Path Rel File]
    -> ([EasyId] -> [EasyId] -> [EasyId]) -- ^ Something that chooses the background ids.
    -> SignatureInferenceStrategy
splitInferAlg name fs func =
    SignatureInferenceStrategy
    { sigInfStratName = name
    , sigInfRelevantSources = $(mkRelFile __FILE__) : fs
    , inferSignature =
          \focus scope ->
              let bgSigFuncs = func focus scope
                  makeNamedExps funcs =
                      rights $ map convertToUsableNamedExp funcs
                  fgNExps = makeNamedExps focus
                  bgNExps = makeNamedExps bgSigFuncs \\ fgNExps
              in InferredSignature [Node fgNExps [Node bgNExps []]]
    }

breakThroughSplitInferAlg ::
       String
    -> [Path Rel File]
    -> ([EasyId] -> [EasyId] -> [EasyId])
    -> Int
    -> SignatureInferenceStrategy
breakThroughSplitInferAlg name fs func maxDistinctOtherFuncs =
    SignatureInferenceStrategy
    { sigInfStratName = name
    , sigInfRelevantSources = $(mkRelFile __FILE__) : fs
    , inferSignature =
          \focus scope ->
              let scope' = scope \\ focus :: [EasyId]
              in InferredSignature $
                 flip map (func' focus scope') $ \funcs -> Node funcs []
    }
  where
    makeNamedExps funcs = rights $ map convertToUsableNamedExp funcs
    func' :: [EasyId] -> [EasyId] -> [[EasyNamedExp]]
    func' focus scope = do
        grp <- groupsOf maxDistinctOtherFuncs (makeNamedExps $ func focus scope)
        ff <- makeNamedExps focus
        pure $ ff : grp

-- groupsOf 1 ls == map (:[]) ls
groupsOf :: (Eq a, Ord a) => Int -> [a] -> [[a]]
groupsOf 0 _ = [[]]
groupsOf n fs = do
    rest <- nub $ sort <$> groupsOf (n - 1) fs
    new <- [f | f <- fs, f `notElem` rest]
    pure $ new : rest

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
    expr = Paren mempty $ Var mempty (idName eid)
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

replaceVarsWithQuickspecVars :: Eq l => Type l -> Either String (Type l)
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
    replaceTyVars ::
           Eq l => [(Name l, l -> Type l)] -> Type l -> Either String (Type l)
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
