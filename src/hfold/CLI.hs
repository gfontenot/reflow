module Hfold.CLI where

import Options.Applicative

data Config = Config
  { width :: Int
  , path :: FilePath
  }

opts :: ParserInfo Config
opts = info (helper <*> config)
  ( fullDesc
  <> progDesc "Intelligently wrap lines to a given length"
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

    <*> argument str
        ( metavar "PATH"
        <> help "Path to input file. If not provided, hfold will attempt to read from STDIN"
        <> value "-"
        )
