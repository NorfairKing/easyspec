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
    , occurrencesEq
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
                Nothing -> ids {setDiscInfStrat = defaultInferenceStrategy}
                _ -> ids
    res <- discoverRelevantEquations ds
    liftIO $
        putStr $ presentEquations $ map (unqualify $ setDiscQualified ds) res

unqualify :: Unqualification -> EasyEq -> EasyEq
unqualify unq (EasyEq e1 e2) = EasyEq (unqualifyExp e1) (unqualifyExp e2)
  where
    unqualifyExp :: EasyExp -> EasyExp
    unqualifyExp =
        foldExp
            (\l qn -> Var l $ q qn)
            OverloadedLabel
            IPVar
            (\l qn -> Con l $ q qn)
            Lit
            (\l el1 qop el2 -> InfixApp l el1 (qo qop) el2)
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
        qo :: QOp () -> QOp ()
        qo (QVarOp l qn) = QVarOp l $ q qn
        qo (QConOp l qn) = QConOp l $ q qn
        q :: QName () -> QName ()
        q c@(Qual l mn n) =
            case unq of
                UnqualifyNothing -> c
                UnqualifyLocal mn' ->
                    if mn == mn'
                        then UnQual l n
                        else c
                UnqualifyAll -> UnQual l n
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
    debug1 "Inferred signature."
    debug1 "Starting to run easyspec now."
    allEqs <- ordNub <$> runEasySpec ds iSig
    debug1 "Found all these equations:"
    debug1 "==[Start of Equations]=="
    debug1 $ unlines $ map prettyEasyEq allEqs
    debug1 "==[End of Equations]=="
    pure allEqs

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
