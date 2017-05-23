{-# LANGUAGE TypeApplications #-}

module EasySpec.Discover.SignatureInference.Monomorphisation where

import Import

import Language.Haskell.Exts.Syntax

import EasySpec.Discover.CodeUtils

{-# ANN module "HLint: ignore Use const" #-}

monomorphise ::
       Eq l
    => [Type l] -- Full types available for monomorphisation
    -> Type l -- Type to monomorphise
    -> [Type l] -- The possible monomorphisations. Currently only completely free variables
monomorphise ts = fillIn @() (findAllTypesAndSubtypes ts)

-- All types and subtypes
findAllTypesAndSubtypes :: (Eq l, Monoid t) => [Type l] -> [(Type l, Kind t)]
findAllTypesAndSubtypes = undefined

fillIn ::
       (Eq t, Eq l, Monoid t)
    => [(Type l, Kind t)] -- Exact types available for monomorphisation
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

getKindedTyVars :: Monoid t => Type l -> ([(Name l, Kind t)], Context t) -- TODO might need to add reevant constraints
getKindedTyVars t =
    ( foldType
          (\_ _ _ -> id)
          (\_ -> (++))
          (\_ _ -> concat)
          (\_ -> id)
          (\_ -> id)
          (\_ -> (++)) -- app
          (\_ n -> [(n, KindStar mempty)])
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
    , CxEmpty mempty)
