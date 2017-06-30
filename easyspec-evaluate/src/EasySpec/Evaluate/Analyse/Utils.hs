{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EasySpec.Evaluate.Analyse.Utils where

import Import

import Data.Tuple (swap)

import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import Data.Csv as CSV

import qualified Data.Vector as V

import qualified Data.ByteString.Lazy as LB

import Development.Shake
import Development.Shake.Path

combineCSVFiles ::
       forall a. (FromNamedRecord a, ToNamedRecord a, DefaultOrdered a)
    => Path Abs File
    -> [Path Abs File]
    -> Rules ()
combineCSVFiles res ins =
    res $%> do
        needP ins
        putLoud $
            unlines
                (unwords
                     ["Combining the following CSV files into", toFilePath res] :
                 map toFilePath ins)
        ls <- concat <$> mapM readCSV ins :: Action [a]
        writeCSV res ls

readCSV :: (FromNamedRecord a, MonadIO m) => Path Abs File -> m [a]
readCSV file = do
    contents <- liftIO $ LB.readFile $ toFilePath file
    case CSV.decodeByName contents of
        Left err ->
            fail $
            unlines
                [ "Failed to read CSV file"
                , toFilePath file
                , "with error:"
                , err
                , show contents
                ]
        Right (_, res) -> pure $ V.toList res

writeCSV ::
       (ToNamedRecord a, DefaultOrdered a, MonadIO m)
    => Path Abs File
    -> [a]
    -> m ()
writeCSV file records = do
    ensureDir $ parent file
    liftIO $
        LB.writeFile (toFilePath file) $ CSV.encodeDefaultOrderedByName records

readJSON :: (FromJSON a, MonadIO m) => Path Abs File -> m a
readJSON file = do
    contents <- liftIO $ LB.readFile $ toFilePath file
    case JSON.eitherDecode contents of
        Left err ->
            fail $
            unwords
                [ "Failed to read JSON file"
                , toFilePath file
                , "with error:"
                , err
                ]
        Right res -> pure res

writeJSON :: (ToJSON a, MonadIO m) => Path Abs File -> a -> m ()
writeJSON file dat = do
    ensureDir $ parent file
    liftIO $ LB.writeFile (toFilePath file) $ JSON.encodePretty dat

byCopying :: Path Abs File -> Path Abs File -> Rules ()
byCopying t f = t $%> copyFileChanged (toFilePath f) (toFilePath t)

orderedCombinationsWithoutSelfCombinations :: [a] -> [(a, a)]
orderedCombinationsWithoutSelfCombinations xs =
    unorderedCombinationsWithoutSelfCombinations xs ++
    map swap (unorderedCombinationsWithoutSelfCombinations xs)

unorderedCombinationsWithoutSelfCombinations :: [a] -> [(a, a)]
unorderedCombinationsWithoutSelfCombinations ls =
    concatMap
        (\t ->
             case t of
                 [] -> []
                 (l1:l2s) -> map ((,) l1) l2s)
        (tails ls)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 func (a, b, c) = func a b c

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 func (a, b, c, d) = func a b c d

uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 func (a, b, c, d, e) = func a b c d e
