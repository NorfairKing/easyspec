{-# LANGUAGE NoImplicitPrelude #-}

module PathExample where

import Prelude (Maybe, FilePath, Bool(..))

import Data.Maybe (fromMaybe)

import Path (parseRelFile, Path, Rel, File, fromRelFile)

parseRel :: FilePath -> Maybe (Path Rel File)
parseRel = parseRelFile
