module EasySpec.Discover.SignatureInference.Monomorphisation where

import Import

import Language.Haskell.Exts.Syntax

import EasySpec.Discover.CodeUtils

{-# ANN module "HLint: ignore Use const" #-}

monomorphise
    :: (Show l, Eq l, Monoid l)
    => [Type l] -- Full types available for monomorphisation
    -> Type l -- Type to monomorphise
    -> [Type l] -- The possible monomorphisations. Currently only completely free variables
monomorphise ts = fillIn (nub $ concatMap findAllTypesAndSubtypes ts)

-- All types and subtypes
findAllTypesAndSubtypes
    :: (Show l, Eq l, Monoid l)
    => Type l -> [(Type l, Kind l)]
findAllTypesAndSubtypes = go
  where
    go t =
        case t of
            TyForall _ _ _ t' -> go t'
            TyFun _ t1 t2 -> go t1 ++ go t2
            TyCon l _ -> [(t, KindStar l)]
            _ -> []

fillIn
    :: (Show l, Eq l, Monoid l)
    => [(Type l, Kind l)] -- Exact types available for monomorphisation
    -> Type l -- Type to monomorphise
    -> [Type l] -- The possible monomorphisations. Currently only completely free variables
fillIn env t =
    let (vars, ctx) = getKindedTyVars t --foldType
    in catMaybes $
       foldType
           (\_ _ _ -> id)
           (\_ -> (++))
           (\_ _ -> concat)
           (\_ -> id)
           (\_ -> id)
           (\_ -> (++)) -- app
           (\_ n ->
                case lookup n vars of
                    Nothing -> [] -- Should not happen, but not a problem if it does.
                    Just k ->
                        flip map env $ \(rt, rk) ->
                            case ctx of
                                CxEmpty _ ->
                                    if rk == k
                                        then Just rt
                                        else Nothing
                                _ -> Nothing -- TODO check if the context is satisfiable by the replacements, then do the above
            )
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
           t

newtype ContextM l = ContextM
    { unContextM :: Context l
    } deriving (Show, Eq, Ord)

instance Monoid l =>
         Monoid (ContextM l) where
    mempty = ContextM $ CxEmpty mempty
    mappend (ContextM c1) (ContextM c2) =
        ContextM $
        case (c1, c2) of
            (CxEmpty _, _) -> c2
            (_, CxEmpty _) -> c1
            (CxSingle l1 ast1, CxSingle l2 ast2) ->
                CxTuple (l1 `mappend` l2) [ast1, ast2]
            (CxSingle l1 asth, CxTuple l2 asts) ->
                CxTuple (l1 `mappend` l2) (asth : asts)
            (CxTuple l1 asts, CxSingle l2 astl) ->
                CxTuple (l1 `mappend` l2) (asts ++ [astl])
            (CxTuple l1 asts1, CxTuple l2 asts2) ->
                CxTuple (l1 `mappend` l2) (asts1 ++ asts2)

getKindedTyVars
    :: (Eq l, Monoid l)
    => Type l -> ([(Name l, Kind l)], Context l)
getKindedTyVars t =
    let (tups, ContextM ctx) =
            runWriter $
            runReaderT
                (foldType
                     (\_ _ mctx b -> do
                          case mctx of
                              Nothing -> pure ()
                              Just c -> tell $ ContextM c
                          b)
                     (\_ -> liftA2 (++))
                     (\_ _ bs -> concat <$> sequence bs)
                     (\_ -> id)
                     (\_ -> id)
                     (\_ rt1 rt2 -- app
                       -> do
                          t1 <- local (KindFn mempty (KindStar mempty)) rt1
                          t2 <- rt2
                          pure $ t1 ++ t2)
                     (\_ n -> do
                          k <- ask
                          pure [(n, k)])
                     (\_ _ -> pure [])
                     (\_ -> id)
                     (\_ v1 _ v2 -> (++) <$> v1 <*> v2)
                     (\_ vs k -> local (const k) vs)
                     (\_ _ -> pure [])
                     (\_ -> liftA2 (++))
                     (\_ _ -> pure [])
                     (\_ _ _ -> id)
                     (\_ _ -> pure [])
                     (\_ _ _ -> pure [])
                     t)
                (KindStar mempty)
    in (nub tups, ctx)
