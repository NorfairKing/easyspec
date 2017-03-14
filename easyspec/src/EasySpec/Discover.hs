{-# LANGUAGE FlexibleContexts #-}

module EasySpec.Discover where

import Import

import DynFlags hiding (Settings)
import GHC
import GHC.Paths (libdir)

import EasySpec.OptParse

import EasySpec.Discover.Gather
import EasySpec.Discover.Utils

discover
    :: (MonadIO m, MonadReader Settings m)
    => DiscoverSettings -> m ()
discover ds = do
    ids <- getIds ds
    runEasySpec ds ids

runEasySpec
    :: MonadIO m
    => DiscoverSettings -> [GHC.Id] -> m ()
runEasySpec ds ids =
    liftIO $
    runGhc (Just libdir) $
            -- Make the quickspec signature code
            -- For some reason I have to restart the session
     do
        initGhcMonad (Just libdir)
        dflags <- getSessionDynFlags
        let intdfflags =
                (prepareFlags dflags)
                {hscTarget = HscInterpreted, ghcLink = LinkInMemory}
        setDFlagsNoLinking intdfflags
            -- This star is necessary so GHC uses the sources instead of the already compiled .o files.
            -- See these:
            -- - https://stackoverflow.com/questions/12790341/haskell-ghc-dynamic-compliation-only-works-on-first-compile
            -- - https://mail.haskell.org/pipermail/glasgow-haskell-users/2011-October/021009.html
        target <- guessTarget ("*" ++ toFilePath (setDiscFile ds)) Nothing
        setTargets [target]
        loadSuccessfully LoadAllTargets
        let imp = IIDecl . GHC.simpleImportDecl . GHC.mkModuleName
        let qsModules =
                [ imp "Test.QuickSpec"
                , imp "Test.QuickSpec.Signature"
                , imp "Prelude"
                , IIModule $ getTargetModName ds
                ]
        setContext qsModules
        getBindings >>= printO
        quickspecSig <- createQuickspecSig ids
        liftIO $ putStrLn quickspecSig
        void $
            execStmt (unwords ["quickSpec", "(", quickspecSig, ")"]) execOptions
