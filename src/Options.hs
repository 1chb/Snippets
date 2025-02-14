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
    development :: Bool
  }
  deriving (Show)

getOptions :: IO Options
getOptions = execParser optsParserInfo

optionsParser :: Parser Options
optionsParser = fmap replaceMagic $ Options <$> portOption <*> developmentSwitch
  where
    portOption =
      option auto $
        long "port"
          <> short 'p'
          <> metavar "PORT"
          <> help "Port to listen on"
          <> value magicPort -- Hack to make default depend on the --development switch
          <> showDefaultWith (const "443, or 8080 if --development is set")
    magicPort = -1
    replaceMagic opts
      | port opts == magicPort = opts {port = if development opts then 8080 else 443}
      | otherwise = opts
    developmentSwitch =
      switch $
        long "development"
          <> help "Run a development server using the http protocol"
          <> short 'd'

optsParserInfo :: ParserInfo Options
optsParserInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> header "Snippets App - a demonstration of a full featured web server"
        <> progDesc "This is a sample application"
    )
