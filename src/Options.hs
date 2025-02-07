module Options (Options (..), getOptions) where

import Options.Applicative (Parser, ParserInfo, execParser, fullDesc, header, help, helper, info, long, progDesc, short, switch, (<**>))

-- Define a data type to hold the command-line options
data Options = Options
  { port :: Int,
    local :: Bool
  }
  deriving (Show)

-- Parser for the command-line options
optionsParser :: Parser Options
optionsParser =
  Options 8080
    <$> switch
      ( long "local"
          <> help "Run a local server using the http protocol on port 8080"
          <> short 'l'
      )

-- ParserInfo for the command-line options
optsParserInfo :: ParserInfo Options
optsParserInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "This is a sample application with a --local flag"
        <> header "Snippets App - a demonstration of a full feature web server"
    )

getOptions :: IO Options
getOptions = execParser optsParserInfo
