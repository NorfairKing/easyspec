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
    , discoverEquations
    , getEasyIds
    , inferenceStrategies
    , mentionsEq
    ) where

import Import

import Language.Haskell.Exts.Pretty (prettyPrint)

import EasySpec.OptParse.Types

import EasySpec.Discover.CodeUtils
import EasySpec.Discover.GatherFromGHC
import EasySpec.Discover.QuickSpec
import EasySpec.Discover.SignatureInference
import EasySpec.Discover.SourceGathering
import EasySpec.Discover.TypeTranslation
import EasySpec.Discover.Types
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
    -- withCurrentDir (inputSpecBaseDir $ setDiscInputSpec ds) $ do
    allEqs <- discoverEquations ds
    let res =
            case setDiscFun ds of
                Nothing -> allEqs
                Just focus -> filter (mentionsEq focus) allEqs
    liftIO $
        mapM_
            (\(EasyEq lh rh) ->
                 putStrLn $ prettyPrint lh ++ " = " ++ prettyPrint rh)
            res

mentionsEq :: EasyQName -> EasyEq -> Bool
mentionsEq n (EasyEq e1 e2) = mentions n e1 || mentions n e2

discoverEquations ::
       (MonadIO m, MonadMask m, MonadReader Settings m)
    => DiscoverSettings
    -> m [EasyEq]
discoverEquations ds = do
    ids <- getEasyIds $ setDiscInputSpec ds
    debug1 "Gathered signature:"
    debug1 $ unlines $ map prettyEasyId ids
    let (focusIds, bgIds) = splitFocus ds ids
    let iSig = inferSignature (setDiscInfStrat ds) focusIds bgIds
    allEqs <- runEasySpec ds iSig
    pure $ nub allEqs

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
