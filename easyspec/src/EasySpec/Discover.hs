{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|

Property discovery happens in multiple steps:

- All the relevant 'GHC.Id's are gathered from a given source file.
- The 'GHC.Id's are converted to 'EasyId's
- The EasyId's are converted to an 'EasyExp' that represents the signature as input to quickspec
- Quickspec is run interactively

-}
module EasySpec.Discover
    ( discover
    , discoverRelevantEquations
    , discoverEquations
    , getEasyIds
    , inferenceStrategies
    , mentionsEq
    ) where

import Import

import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.Syntax

import EasySpec.OptParse.Types

import EasySpec.Discover.CodeUtils
import EasySpec.Discover.GatherFromGHC
import EasySpec.Discover.QuickSpec
import EasySpec.Discover.SignatureInference
import EasySpec.Discover.SourceGathering
import EasySpec.Discover.TypeTranslation
import EasySpec.Discover.Types
import EasySpec.Discover.Utils
import EasySpec.Utils

discover ::
       (MonadIO m, MonadMask m, MonadReader Settings m)
    => DiscoverSettings
    -> m ()
discover ids = do
    let ds =
            case setDiscFun ids of
                Nothing -> ids {setDiscInfStrat = inferFullBackground}
                _ -> ids
    res <- discoverRelevantEquations ds
    liftIO $
        putStr $
        presentEquations $ bool id (map unqualify) (setDiscQualified ds) res

unqualify :: EasyEq -> EasyEq
unqualify (EasyEq e1 e2) = EasyEq (unqualifyExp e1) (unqualifyExp e2)
  where
    unqualifyExp :: EasyExp -> EasyExp
    unqualifyExp =
        foldExp
            (\l qn -> Var l $ q qn)
            OverloadedLabel
            IPVar
            (\l qn -> Con l $ q qn)
            Lit
            InfixApp
            App
            NegApp
            Lambda
            Let
            If
            MultiIf
            Case
            Do
            MDo
            Tuple
            TupleSection
            List
            ParArray
            Paren
            LeftSection
            RightSection
            RecConstr
            RecUpdate
            EnumFrom
            EnumFromTo
            EnumFromThen
            EnumFromThenTo
            ParArrayFromTo
            ParArrayFromThenTo
            ListComp
            ParComp
            ParArrayComp
            ExpTypeSig
            VarQuote
            TypQuote
            BracketExp
            SpliceExp
            QuasiQuote
            TypeApp
            XTag
            XETag
            XPcdata
            XExpTag
            XChildTag
            CorePragma
            SCCPragma
            GenPragma
            Proc
            LeftArrApp
            RightArrApp
            LeftArrHighApp
            RightArrHighApp
            LCase
            ExprHole
      where
        q :: QName l -> QName l
        q (Qual l _ n) = UnQual l n
        q c = c

presentEquations :: [EasyEq] -> String
presentEquations =
    unlines . map (\(EasyEq lh rh) -> prettyPrint lh ++ " = " ++ prettyPrint rh)

discoverRelevantEquations ::
       (MonadIO m, MonadMask m, MonadReader Settings m)
    => DiscoverSettings
    -> m [EasyEq]
discoverRelevantEquations ds = do
    allEqs <- discoverEquations ds
    pure $
        case setDiscFun ds of
            Nothing -> allEqs
            Just focus -> filter (mentionsEq focus) allEqs

mentionsEq :: EasyQName -> EasyEq -> Bool
mentionsEq n (EasyEq e1 e2) = mentions n e1 || mentions n e2

discoverEquations ::
       (MonadIO m, MonadMask m, MonadReader Settings m)
    => DiscoverSettings
    -> m [EasyEq]
discoverEquations ds = do
    ids <- getEasyIds $ setDiscInputSpec ds
    debug1 "Gathered scope:"
    debug1 $ unlines $ map prettyEasyId ids
    let (focusIds, bgIds) = splitFocus ds ids
    let iSig = inferSignature (setDiscInfStrat ds) focusIds bgIds
    debug1 "Inferred signature:"
    debug1 $ prettyInferredSignature iSig
    debug1 "Starting to run easyspec now."
    allEqs <- runEasySpec ds iSig
    pure $ ordNub allEqs

getEasyIds ::
       (MonadIO m, MonadMask m, MonadReader Settings m)
    => InputSpec
    -> m [EasyId]
getEasyIds is = do
    idDatas <- getGHCIds is
    dats <-
        forM idDatas $ \idData -> do
            mimpl <- gatherSourceOf is idData
            pure (idData, mimpl)
    pure $ map (uncurry toEasyId) dats

splitFocus :: DiscoverSettings -> [EasyId] -> ([EasyId], [EasyId])
splitFocus ds ids =
    let fs =
            case find (\i -> Just (idName i) == setDiscFun ds) ids of
                Nothing -> []
                Just i -> [i]
    in (fs, ids \\ fs)
