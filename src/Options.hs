module Options (Options (..), getOptions) where

import Options.Applicative
  ( Parser,
    ParserInfo,
    auto,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    progDesc,
    short,
    showDefaultWith,
    switch,
    value,
    (<**>),
  )

data Options = Options
  { port :: Int,
    local :: Bool
  }
  deriving (Show)

getOptions :: IO Options
getOptions = execParser optsParserInfo

optionsParser :: Parser Options
optionsParser = fmap replaceMagic $ Options <$> portOption <*> localSwitch
  where
    portOption =
      option auto $
        long "port"
          <> short 'p'
          <> metavar "PORT"
          <> help "Port to listen on"
          <> value magicPort -- Hack to make default depend on the --local switch
          <> showDefaultWith (const "443, or 8080 if --local is set")
    magicPort = -1
    replaceMagic opts
      | port opts == magicPort = opts {port = if local opts then 8080 else 443}
      | otherwise = opts
    localSwitch =
      switch $
        long "local"
          <> help "Run a local server using the http protocol"
          <> short 'l'

optsParserInfo :: ParserInfo Options
optsParserInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> header "Snippets App - a demonstration of a full featured web server"
        <> progDesc "This is a sample application"
    )
