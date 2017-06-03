{-# LANGUAGE FlexibleContexts #-}

module EasySpec.Discover.QuickSpec where

import Import

import Control.Monad.State
import Data.Dynamic
import Data.List.Split
import Data.Tree

import DynFlags hiding (Settings)
import GHC hiding (Qual, Name)
import GHC.LanguageExtensions
import GHC.Paths (libdir)
import Name

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax

import EasySpec.OptParse.Types
import EasySpec.Utils

import EasySpec.Discover.CodeUtils
import EasySpec.Discover.SignatureGeneration
import EasySpec.Discover.Types
import EasySpec.Discover.Utils

runEasySpec ::
       (MonadIO m, MonadReader Settings m)
    => DiscoverSettings
    -> InferredSignature
    -> m [EasyEq]
runEasySpec ds iSig = do
    sets <- ask
    liftIO $ do
        let sourceFile = inputSpecAbsFile $ setDiscInputSpec ds
        runGhc (Just libdir) $ do
            initGhcMonad (Just libdir)
            dflags <- getSessionDynFlags
            let intdfflags =
                    (addTypeClassExts $ prepareFlags dflags)
                    { hscTarget = HscInterpreted
                    , ghcLink = LinkInMemory
                    , importPaths =
                          importPaths dflags ++
                          [toFilePath $ inputSpecBaseDir $ setDiscInputSpec ds]
                    }
            setDFlagsNoLinking intdfflags
                -- This star is necessary so GHC uses the sources instead of the already compiled .o files.
                -- See these:
                -- - https://stackoverflow.com/questions/12790341/haskell-ghc-dynamic-compliation-only-works-on-first-compile
                -- - https://mail.haskell.org/pipermail/glasgow-haskell-users/2011-October/021009.html
            target <- guessTarget ("*" ++ toFilePath sourceFile) Nothing
            setTargets [target]
            loadSuccessfully LoadAllTargets
            let imp = GHC.simpleImportDecl . GHC.mkModuleName
            let qsModules =
                    [ IIDecl $ (imp "QuickSpec") {ideclQualified = True}
                    , IIDecl $ (imp "QuickSpec.Eval") {ideclQualified = True}
                    , IIDecl $
                      (imp "QuickSpec.Signature") {ideclQualified = True}
                    , IIDecl $ (imp "QuickSpec.Term") {ideclQualified = True}
                    , IIDecl $
                      (imp "Text.PrettyPrint.HughesPJClass")
                      {ideclQualified = True}
                    , IIDecl $
                      (imp "Text.PrettyPrint.HughesPJ") {ideclQualified = True}
                    , IIDecl $ (imp "Data.Maybe") {ideclQualified = True}
                    , IIDecl $ (imp "Data.Monoid") {ideclQualified = True}
                    , IIDecl $ imp "Prelude"
                    , IIModule $
                      getTargetModName $ inputSpecFile $ setDiscInputSpec ds
                    ]
            setContext qsModules
            let declaretc =
                    unlines
                        [ "let typeclass :: (c => a) -> QuickSpec.Dict c -> a"
                        , "    typeclass x QuickSpec.Dict = x"
                        ]
            void $ execStmt declaretc execOptions
            runReaderT (runQuickspecOn iSig) sets

runQuickspecOn :: GhcMonad m => InferredSignature -> ReaderT Settings m [EasyEq]
runQuickspecOn iSig = do
    let expForest = makeSignatureExpressions iSig
    fmap concat $
        forM expForest $ \expTree ->
            execWriterT $ evalStateT (go expTree) (0 :: Int)
  where
    liftGHC = lift . lift . lift
    nextSigExpName = do
        num <- get
        modify (+ 1)
        pure $ Ident mempty $ "s" ++ show num
    -- go :: GhcMonad m => Tree EasyExp -> StateT Int (WriterT [EasyEq] (ReaderT Settings m) EasyExp
    exec s = do
        debug1 s
        res <- liftGHC $ execStmt s execOptions
        case res of
            ExecComplete eres _ -> do
                debug1 "Done:"
                case eres of
                    Left e -> liftIO $ print e
                    Right ns -> debug1 $ show $ map Name.getOccString ns
            ExecBreak ns _ -> do
                debug1 "Broke:"
                debug1 $ show $ map Name.getOccString ns
    go (Node curSigExp others) = do
        bgExps <- mapM go others
        let sigExp = mconcatSigsExp $ curSigExp : bgExps
        debug1 "Running quickspec with signature:"
        debug1 "==[Start of Signature Expression]=="
        debug1 $ prettyPrint sigExp
        debug1 "==[End of Signature Expression]=="
        let quickSpecExp = runQuickspecExp sigExp
        resName <- nextSigExpName
        let stmt = bindTo resName quickSpecExp
        exec $ prettyPrintOneLine stmt
        let resExp = Var mempty (UnQual mempty resName)
        eqs <- ordNub <$> getEqs resExp
        tell eqs
        debug1 "Found these equations:"
        debug1 "==[Start of Equations]=="
        debug1 $ unlines $ map prettyEasyEq eqs
        debug1 "==[End of Equations]=="
        pure resExp
    getEqs resExp = do
        let showBackgroundExp = showPrettyBackgroundExp resExp
        let expStr = prettyPrintOneLine showBackgroundExp
        dyn <- liftGHC $ dynCompileExpr expStr
        case fromDynamic dyn of
            Nothing ->
                liftIO $
                die $ unwords ["failed to coerce the string result of", expStr]
            Just res ->
                forM res $ \s ->
                    let splitStr = " = "
                    in case splitOn splitStr s of
                           [lhs, rhs] ->
                               case (,) <$> parseExp lhs <*> parseExp rhs of
                                   ParseFailed srcloc err ->
                                       liftIO $
                                       die $
                                       unwords
                                           [ "Failed to parse one of two expressions:"
                                           , show lhs
                                           , "and"
                                           , show rhs
                                           , "at"
                                           , show srcloc
                                           , "with error:"
                                           , err
                                           ]
                                   ParseOk (lh, rh) ->
                                       pure $ EasyEq (() <$ lh) (() <$ rh)
                           ss ->
                               liftIO $
                               die $
                               unwords
                                   [ "failed to split an equation"
                                   , s
                                   , "into two pieces"
                                   , s
                                   , "on"
                                   , show splitStr
                                   , "got"
                                   , show ss
                                   , "instead."
                                   ]

bindTo :: EasyName -> EasyExp -> EasyStmt
bindTo n = Generator mempty (PVar mempty n)

makeSignatureExpressions :: InferredSignature -> Forest EasyExp
makeSignatureExpressions (InferredSignature t) = mapF createQuickspecSig t

mapF :: (a -> b) -> Forest a -> Forest b
mapF func = map (fmap func)

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
