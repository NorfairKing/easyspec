{-# LANGUAGE RecordWildCards #-}

module EasySpec.Discover.QuickSpec where

import Import

import Data.Dynamic

import DynFlags hiding (Settings)
import GHC hiding (Qual, Name)
import GHC.LanguageExtensions
import GHC.Paths (libdir)

import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax

import EasySpec.OptParse

import EasySpec.Discover.SignatureGeneration
import EasySpec.Discover.Types
import EasySpec.Discover.Utils

runEasySpec :: MonadIO m => DiscoverSettings -> InferredSignature -> m [String]
runEasySpec ds iSig =
    liftIO $
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
                , IIDecl $ (imp "QuickSpec.Signature") {ideclQualified = True}
                , IIDecl $ (imp "QuickSpec.Term") {ideclQualified = True}
                , IIDecl $
                  (imp "Text.PrettyPrint.HughesPJClass") {ideclQualified = True}
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
        runQuickspecOn iSig

runQuickspecOn :: GhcMonad m => InferredSignature -> m [String]
runQuickspecOn iSig = do
    (bgSig, focusSig) <- liftIO $ makeSignatureExpressions iSig
    let s1name = Ident mempty "s1"
    void $
        execStmt
            (prettyPrintOneLine $ bindTo s1name $ runQuickspecExp bgSig)
            execOptions
    let s1exp = Var mempty (UnQual mempty s1name)
    let s2name = Ident mempty "s2"
    void $
        execStmt
            (prettyPrintOneLine $
             bindTo s2name $ runQuickspecExp $ mappendSigsExp s1exp focusSig)
            execOptions
    let s2exp = Var mempty (UnQual mempty s2name)
    -- compiledExpr <- compileExprRemote $ showPrettyBackgroundExp s2exp
    -- evalString env compiledExpr
    let expStr = prettyPrintOneLine $ showPrettyBackgroundExp s2exp
    dyn <- dynCompileExpr expStr
    case fromDynamic dyn of
        Nothing ->
            liftIO $
            die $ unwords ["failed to coerce the string result of", expStr]
        Just res -> pure res

bindTo :: EasyName -> EasyExp -> EasyStmt
bindTo n = Generator mempty (PVar mempty n)

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
