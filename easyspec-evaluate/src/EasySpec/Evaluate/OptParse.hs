module EasySpec.Evaluate.OptParse
    ( module EasySpec.Evaluate.OptParse
    , Instructions
    , Dispatch(..)
    , Settings(..)
    ) where

import Import

import System.Environment (getArgs)

import Options.Applicative

import qualified EasySpec.Discover.Types as ES
import EasySpec.Utils as ES (isSourceFile)

import EasySpec.Evaluate.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
    (cmd, flags) <- getArguments
    config <- getConfiguration cmd flags
    combineToInstructions cmd flags config

combineToInstructions :: Command -> Flags -> Configuration -> IO Instructions
combineToInstructions cmd Flags Configuration = (,) <$> disp <*> pure Settings
  where
    disp =
        case cmd of
            CommandEvaluate mfilepath mdirpath -> do
                let dirpath = fromMaybe "examples" mdirpath
                bd <- resolveDir' dirpath
                fs <-
                    case mfilepath of
                        Just f -> do
                            af <- resolveFile' f
                            rf <- makeRelative bd af
                            pure [rf]
                        Nothing ->
                            (mapMaybe (makeRelative bd) .
                             filter ES.isSourceFile . snd) <$>
                            listDirRecur bd
                pure $ DispatchEvaluate $ map (ES.InputSpec bd) fs
            CommandBuild target -> pure $ DispatchBuild target
            CommandBuildEverything -> pure DispatchBuildEverything

getConfiguration :: Command -> Flags -> IO Configuration
getConfiguration _ _ = pure Configuration

getArguments :: IO Arguments
getArguments = do
    args <- getArgs
    let result = runArgumentsParser args
    handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argParser
  where
    prefs_ =
        ParserPrefs
        { prefMultiSuffix = ""
        , prefDisambiguate = True
        , prefShowHelpOnError = True
        , prefShowHelpOnEmpty = True
        , prefBacktrack = True
        , prefColumns = 80
        }

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) help_
  where
    help_ = fullDesc <> progDesc description
    description = "Easyspec evaluate"

parseArgs :: Parser Arguments
parseArgs = (,) <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand =
    hsubparser $
    mconcat
        [ command "evaluate" parseCommandEvaluate
        , command "build" parseCommandBuild
        , command "build-everything" parseCommandBuildEverything
        ]

parseCommandEvaluate :: ParserInfo Command
parseCommandEvaluate = info parser modifier
  where
    parser =
        CommandEvaluate <$>
        argument
            (Just <$> str)
            (mconcat
                 [ metavar "FILE"
                 , value Nothing
                 , help "the path to a file that is an example"
                 ]) <*>
        argument
            (Just <$> str)
            (mconcat
                 [ metavar "DIR"
                 , value Nothing
                 , help
                       "the path to a directory that is the base director of examples"
                 ])
    modifier =
        fullDesc <>
        progDesc "Evaluate different easyspec strategies on examples."

parseCommandBuild :: ParserInfo Command
parseCommandBuild = info parser modifier
  where
    parser =
        CommandBuild <$>
        (many $ strArgument (mconcat [metavar "TARGET", help "the target to build."]))
    modifier = fullDesc <> progDesc "Build a target in the evaluation system."

parseCommandBuildEverything :: ParserInfo Command
parseCommandBuildEverything = info parser modifier
  where
    parser = pure CommandBuildEverything
    modifier =
        fullDesc <> progDesc "Build all targets in the evaluation system."

parseFlags :: Parser Flags
parseFlags = pure Flags
