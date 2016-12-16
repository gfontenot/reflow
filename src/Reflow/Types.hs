module Reflow.Types where

import Data.Text (Text)

data Config = Config
  { width :: Int
  , ignoreHeaders :: Bool
  , path :: FilePath
  }

lessWidth :: Int -> Config -> Config
lessWidth i c = c { width = width c - i }

data Content = Normal Int Text | Blank | Header Text | Quoted Content | CodeBlock Text | PGPBlock Text
    deriving (Show)
