module Reflow.CLI where

import Options.Applicative

import Reflow.Types

opts :: ParserInfo Config
opts = info (helper <*> config)
  ( fullDesc
  <> progDesc "Intelligently reflow text to a given length"
  )

config :: Parser Config
config = Config
    <$> option auto
        ( long "width"
        <> short 'w'
        <> metavar "WIDTH"
        <> value 80
        <> help "Specify a line width to use instead of the default 80 columns"
        )

    <*> switch
        ( long "ignore-headers"
        <> help "Don't wrap email headers"
        )

    <*> argument str
        ( metavar "PATH"
        <> help "Path to input file. If not provided, reflow will attempt to read from STDIN"
        <> value "-"
        )
