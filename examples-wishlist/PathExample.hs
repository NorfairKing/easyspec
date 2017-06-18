{-# LANGUAGE NoImplicitPrelude #-}

module PathExample where

import Prelude (Bool(..), FilePath, Maybe)

import Data.Maybe (fromMaybe)

import Path (File, Path, Rel, fromRelFile, parseRelFile)

parseRel :: FilePath -> Maybe (Path Rel File)
parseRel = parseRelFile
