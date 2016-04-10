module Reflow.Types where

import Data.Text (Text)

data Config = Config
  { width :: Int
  , ignoreHeaders :: Bool
  , path :: FilePath
  }

data Content = Normal Text | Header Text | Quoted Text | CodeBlock Text
    deriving (Show)
