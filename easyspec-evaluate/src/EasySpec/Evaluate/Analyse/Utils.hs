{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EasySpec.Evaluate.Analyse.Utils where

import Import

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
