{-# LANGUAGE RecordWildCards #-}

module EasySpec.Discover.QuickSpec where

import Import

import DynFlags hiding (Settings)
import GHC hiding (Qual)
import GHC.LanguageExtensions
import GHC.Paths (libdir)

import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax

import EasySpec.OptParse

import EasySpec.Discover.SignatureGeneration
import EasySpec.Discover.Types
import EasySpec.Discover.Utils

runEasySpec :: MonadIO m => DiscoverSettings -> InferredSignature -> m ()
runEasySpec ds iSig =
    liftIO $ do
        (bgSig, focusSig) <- makeSignatureExpressions iSig
        runGhc (Just libdir) $ do
            initGhcMonad (Just libdir)
            dflags <- getSessionDynFlags
            let intdfflags =
                    (addTypeClassExts $ prepareFlags dflags)
                    {hscTarget = HscInterpreted, ghcLink = LinkInMemory}
            setDFlagsNoLinking intdfflags
                -- This star is necessary so GHC uses the sources instead of the already compiled .o files.
                -- See these:
                -- - https://stackoverflow.com/questions/12790341/haskell-ghc-dynamic-compliation-only-works-on-first-compile
                -- - https://mail.haskell.org/pipermail/glasgow-haskell-users/2011-October/021009.html
            target <- guessTarget ("*" ++ toFilePath (setDiscFile ds)) Nothing
            setTargets [target]
            loadSuccessfully LoadAllTargets
            let imp = GHC.simpleImportDecl . GHC.mkModuleName
            let qsModules =
                    [ IIDecl $ (imp "QuickSpec") {ideclQualified = True}
                    , IIDecl $ (imp "QuickSpec.Eval") {ideclQualified = True}
                    , IIDecl $
                      (imp "QuickSpec.Signature") {ideclQualified = True}
                    , IIDecl $ (imp "QuickSpec.Term") {ideclQualified = True}
                    , IIDecl $ (imp "Data.Monoid") {ideclQualified = True}
                    , IIDecl $ imp "Prelude"
                    , IIModule $ getTargetModName $ setDiscFile ds
                    ]
            setContext qsModules
            let declaretc =
                    unlines
                        [ "let typeclass :: (c => a) -> QuickSpec.Dict c -> a"
                        , "    typeclass x QuickSpec.Dict = x"
                        ]
            void $ execStmt declaretc execOptions
            let backgroundSigName = Ident mempty "bgSig"
            void $
                execStmt
                    (prettyPrintOneLine $
                     Generator
                         mempty
                         (PVar mempty backgroundSigName)
                         (runQuickspecExp bgSig))
                    execOptions
            void $
                execStmt
                    (prettyPrintOneLine $
                     runQuickspecExp $
                     mappendSigsExp
                         (Var mempty (UnQual mempty backgroundSigName))
                         focusSig)
                    execOptions

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

makeSignatureExpressions :: InferredSignature -> IO (EasyExp, EasyExp)
makeSignatureExpressions InferredSignature {..} =
    (,) <$> m sigBackgroundIds <*> m sigFocusIds
  where
    m is =
        case createQuickspecSig is of
            Nothing ->
                liftIO $
                die
                    "Unable to generate quickspec signature expression: Not enough type variables in quickspec."
            Just e -> pure e

-- prettyPrintOneLine :: _ -> String
prettyPrintOneLine :: Pretty a => a -> String
prettyPrintOneLine =
    prettyPrintStyleMode (style {mode = OneLineMode}) defaultMode

addTypeClassExts :: DynFlags -> DynFlags
addTypeClassExts dflags =
    foldl
        xopt_set
        dflags
        [ ScopedTypeVariables
        , ConstraintKinds
        , RankNTypes
        , ConstraintKinds
        , FlexibleContexts
        ]
