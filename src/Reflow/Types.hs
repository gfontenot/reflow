module Reflow.Types where

import Data.Text (Text)

data Config = Config
  { width :: Int
  , path :: FilePath
  }

data Content = Normal Text | Header Text | Quoted Text | CodeBlock Text
    deriving (Show)
