{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module EasySpec where

import Import

import EasySpec.OptParse

import Data.IORef

import System.FilePath

import DynFlags hiding (Settings)
import GHC
import GHC.LanguageExtensions
import GHC.Paths (libdir)
import HscTypes
import NameEnv
import OccName
import Outputable
import RdrName
import TcRnTypes
import Var

easyspec :: IO ()
easyspec = do
    (disp, sets) <- getInstructions
    runReaderT (dispatch disp) sets

dispatch
    :: (MonadIO m, MonadReader Settings m)
    => Dispatch -> m ()
dispatch (DispatchDiscover ds) = do
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
        setSessionDynFlags intdfflags
            -- This star is necessary so GHC uses the sources instead of the already compiled .o files.
            -- See these:
            -- - https://stackoverflow.com/questions/12790341/haskell-ghc-dynamic-compliation-only-works-on-first-compile
            -- - https://mail.haskell.org/pipermail/glasgow-haskell-users/2011-October/021009.html
        target <- guessTarget ("*" ++ toFilePath (setDiscFile ds)) Nothing
        setTargets [target]
        load LoadAllTargets
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

prepareFlags :: DynFlags -> DynFlags
prepareFlags dflags = foldl xopt_set dflags [Cpp, ImplicitPrelude, MagicHash]

getTargetModName :: DiscoverSettings -> ModuleName
getTargetModName =
    mkModuleName . dropExtension . toFilePath . filename . setDiscFile

getIds
    :: MonadIO m
    => DiscoverSettings -> m [GHC.Id]
getIds ds@DiscoverSettings {..} =
    liftIO $
    runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let compdflags = prepareFlags dflags
        setSessionDynFlags compdflags
        target <- guessTarget (toFilePath setDiscFile) Nothing
        setTargets [target]
        load LoadAllTargets
                    -- Doesn't work in a project, only in top-level modules
        let modname = getTargetModName ds
        printO modname
        modSum <- getModSummary modname
        parsedModule <- parseModule modSum
        tmod <- typecheckModule parsedModule
        let (tcenv, moddets) = tm_internals_ tmod
        let names = concatMap (map gre_name) $ occEnvElts $ tcg_rdr_env tcenv
        fmap catMaybes $
            forM names $ \name -> do
                tything <- lookupName name
                pure $
                    case tything of
                        Just (AnId i) -> Just i
                        Just _ -> Nothing
                        Nothing -> Nothing

createQuickspecSig
    :: GhcMonad m
    => [GHC.Id] -> m String
createQuickspecSig ids = do
    constantExprs <- mapM idConstant ids
    let constantList = intercalate ", " constantExprs
    pure $
        "signature [" ++
        constantList ++ ", " ++ "vars [\"a\"] (undefined :: Char)]"

idConstant
    :: GhcMonad m
    => GHC.Id -> m String
idConstant i = do
    name <- showGHC $ Var.varName i
    pure $ unwords ["fun1", show name, name]

showGHC
    :: (GhcMonad m, Outputable a)
    => a -> m String
showGHC a = do
    dfs <- getProgramDynFlags
    pure $ showPpr dfs a

printO
    :: (GhcMonad m, Outputable a)
    => a -> m ()
printO a = showGHC a >>= (liftIO . putStrLn)
