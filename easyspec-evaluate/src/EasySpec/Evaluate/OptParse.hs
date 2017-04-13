module EasySpec.Evaluate.OptParse
    ( module EasySpec.Evaluate.OptParse
    , Instructions
    , Dispatch(..)
    , Settings(..)
    ) where

import Import

import System.Environment (getArgs)

import Options.Applicative

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
            CommandEvaluate mdirpath -> do
                dir <- resolveDir' $ fromMaybe "examples" mdirpath
                pure $ DispatchEvaluate dir

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
parseCommand = hsubparser $ mconcat [command "evaluate" parseCommandEvaluate]

parseCommandEvaluate :: ParserInfo Command
parseCommandEvaluate = info parser modifier
  where
    parser =
        CommandEvaluate <$>
        option
            (Just <$> str)
            (mconcat
                 [ metavar "DIR"
                 , long "examples-dir"
                 , value Nothing
                 , help "the directory to look for code to evaluate in"
                 ])
    modifier = fullDesc <> progDesc "Command example."

parseFlags :: Parser Flags
parseFlags = pure Flags
