{-# LANGUAGE FlexibleContexts #-}

module EasySpec.Discover.Utils where

import Import

import System.FilePath

import DynFlags hiding (Settings)
import GHC
import GHC.LanguageExtensions
import Outputable
import Var

import EasySpec.OptParse

setDFlagsNoLinking
    :: GhcMonad m
    => DynFlags -> m ()
setDFlagsNoLinking = void . setSessionDynFlags

loadSuccessfully
    :: GhcMonad m
    => LoadHowMuch -> m ()
loadSuccessfully hm = do
    r <- load hm
    case r of
        Succeeded -> pure ()
        Failed -> fail "Loading failed. No idea why."

prepareFlags :: DynFlags -> DynFlags
prepareFlags dflags = foldl xopt_set dflags [Cpp, ImplicitPrelude, MagicHash]

getTargetModName :: DiscoverSettings -> ModuleName
getTargetModName =
    mkModuleName . dropExtension . toFilePath . filename . setDiscFile

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
