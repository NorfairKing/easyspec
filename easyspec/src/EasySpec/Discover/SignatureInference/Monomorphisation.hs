module EasySpec.Discover.SignatureInference.Monomorphisation where

import Import

import Language.Haskell.Exts.Syntax

import EasySpec.Discover.CodeUtils

{-# ANN module "HLint: ignore Use const" #-}

monomorphise ::
       (Eq l, Monoid l)
    => [Type l] -- Full types available for monomorphisation
    -> Type l -- Type to monomorphise
    -> [Type l] -- The possible monomorphisations. Currently only completely free variables
monomorphise ts = fillIn (findAllTypesAndSubtypes ts)

-- All types and subtypes
findAllTypesAndSubtypes :: (Eq l, Monoid l) => [Type l] -> [(Type l, Kind l)]
findAllTypesAndSubtypes = undefined

fillIn ::
       (Eq l, Monoid l)
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
                                        then Nothing
                                        else Just rt
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

getKindedTyVars :: (Eq l, Monoid l) => Type l -> ([(Name l, Kind l)], Context l)
getKindedTyVars t =
    ( nub $
      runReader
          (foldType
               (\_ _ _ -> id)
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
    , CxEmpty mempty)
