{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Discover.SignatureGeneration where

import Import hiding (Alt)

import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax

import EasySpec.Discover.TH
import EasySpec.Discover.Types

{-# ANN module "HLint: ignore Avoid lambda" #-}

createQuickspecSigExp :: [EasyNamedExp] -> EasyExp
createQuickspecSigExp = runQuickspecExp . createQuickspecSig

runQuickspecExp :: EasyExp -> EasyExp
runQuickspecExp = App () $(easyExp "QuickSpec.Eval.quickSpec")

showPrettyBackgroundExp :: EasyExp -> EasyExp
showPrettyBackgroundExp =
    App mempty
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
    App mempty
        (App mempty
             (Var mempty
                  (Qual
                       mempty
                       (ModuleName mempty "QuickSpec.Eval")
                       (Ident mempty "quickSpecWithBackground")))
             bgsig)

mappendSigsExp :: EasyExp -> EasyExp -> EasyExp
mappendSigsExp a =
    App mempty
        (App mempty
             (Var mempty
                  (Qual
                       mempty
                       (ModuleName mempty "Data.Monoid")
                       (Ident mempty "mappend")))
             a)

mconcatSigsExp :: [EasyExp] -> EasyExp
mconcatSigsExp [e] = e
mconcatSigsExp es =
    App mempty
        (Var mempty
             (Qual
                  mempty
                  (ModuleName mempty "Data.Monoid")
                  (Ident mempty "mconcat")))
        (List mempty es)

createQuickspecSig :: (Eq m, Monoid m) => [NamedExp m] -> Exp m
createQuickspecSig nexps =
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
              (List mempty $ map signatureComponent nexps)
        , FieldUpdate -- Just to make tests go a bit quicker
              mempty
              (Qual
                   mempty
                   (ModuleName mempty "QuickSpec.Signature")
                   (Ident mempty "maxTests"))
              (App mempty
                   (Con mempty
                        (Qual
                             mempty
                             (ModuleName mempty "Data.Maybe")
                             (Ident mempty "Just")))
                   (Lit mempty (Int mempty 100 "100")))
        , FieldUpdate -- Keep quickspec silent
              mempty
              (Qual
                   mempty
                   (ModuleName mempty "QuickSpec.Signature")
                   (Ident mempty "silent"))
              (Con mempty
                   (Qual
                        mempty
                        (ModuleName mempty "Prelude")
                        (Ident mempty "True")))
        ]

signatureComponent :: (Eq m, Monoid m) => NamedExp m -> Exp m
signatureComponent (NamedExp funNameStr funExp) =
    App mempty
        (App mempty
             (Var mempty
                  (Qual
                       mempty
                       (ModuleName mempty "QuickSpec.Term")
                       (Ident mempty "constant")))
             (Lit mempty $
              String mempty (prettyPrint funNameStr) (prettyPrint funNameStr)))
        funExp
