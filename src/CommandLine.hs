module CommandLine (cmdParser, CommandLineConfig(..), Command(..)) where

import           Options.Applicative (Parser, ParserInfo, command, fullDesc,
                                      header, help, helper, hsubparser, info,
                                      long, metavar, option, progDesc, short,
                                      str, value, (<**>))

data Command = StartApp | ShowConfig deriving (Show)

data CommandLineConfig = CommandLineConfig {
      clcConfigPath :: Maybe FilePath
    , clcCommand    :: Command
} deriving (Show)

cmdParser :: ParserInfo CommandLineConfig
cmdParser =
  info (optParser <**> helper)
       ( fullDesc
      <> progDesc "Start mobile wallet backend"
      <> header "mobile wallet backend - the backend for mobile wallet" )

optParser :: Parser CommandLineConfig
optParser = CommandLineConfig <$> configParser <*> commandParser

configParser :: Parser (Maybe FilePath)
configParser =
  option ( Just <$> str)
         ( long "config"
        <> metavar "CONFIG"
        <> value Nothing
        <> short 'c'
        <> help "Path to the configuration file." )

commandParser :: Parser Command
commandParser =
  hsubparser $
  mconcat
    [ hparser "start-app" "Start the mobile wallet backend" StartApp
    , hparser "show-config" "Show config" ShowConfig
    ]
  where
    hparser name desc cmd =
      command name $ info (pure cmd) (fullDesc <> progDesc desc)
