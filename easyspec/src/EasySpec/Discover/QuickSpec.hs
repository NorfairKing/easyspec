module EasySpec.Discover.QuickSpec where

import Import

import DynFlags hiding (Settings)
import GHC
import GHC.LanguageExtensions
import GHC.Paths (libdir)

import Language.Haskell.Exts.Pretty

import EasySpec.OptParse

import EasySpec.Discover.Types
import EasySpec.Discover.Utils

runEasySpec :: MonadIO m => DiscoverSettings -> SignatureExpression -> m ()
runEasySpec ds sigExp =
    liftIO $
    runGhc (Just libdir) $
            -- Make the quickspec signature code
            -- For some reason I have to restart the session
     do
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
                , IIDecl $ (imp "QuickSpec.Signature") {ideclQualified = True}
                , IIDecl $ (imp "QuickSpec.Term") {ideclQualified = True}
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
        void $ execStmt (prettySigExp sigExp) execOptions

prettySigExp :: SignatureExpression -> String
prettySigExp (SignatureExpression ee) =
    prettyPrintStyleMode (style {mode = OneLineMode}) defaultMode ee

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
