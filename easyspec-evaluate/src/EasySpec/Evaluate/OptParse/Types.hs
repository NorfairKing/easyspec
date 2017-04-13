module EasySpec.Evaluate.OptParse.Types where

import Import

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

newtype Command =
    CommandEvaluate (Maybe FilePath)
    deriving (Show, Eq)

data Flags =
    Flags
    deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

newtype Dispatch =
    DispatchEvaluate (Path Abs Dir)
    deriving (Show, Eq)

data Settings =
    Settings
    deriving (Show, Eq)
