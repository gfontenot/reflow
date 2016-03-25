{-# LANGUAGE OverloadedStrings #-}

import Options.Applicative
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Config = Config
  { width :: Int
  , path :: FilePath
  }

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

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> config)
      ( fullDesc
      <> progDesc "Intelligently wrap lines to a given length"
      )

run :: Config -> IO ()
run (Config w p) = do
    c <- contentsOrSTDIN p
    T.putStrLn $ T.stripEnd . T.unlines $ wrapLine w =<< T.lines c

contentsOrSTDIN :: FilePath -> IO Text
contentsOrSTDIN "-" = T.getContents
contentsOrSTDIN p = T.readFile p

wrapLine :: Int -> Text -> [Text]
wrapLine _ "" = [""]
wrapLine w s = foldl (splitWordsAt w) [] $ T.words s

splitWordsAt :: Int -> [Text] -> Text -> [Text]
splitWordsAt _ [] x = [x]
splitWordsAt w xs x
  | T.length (appendToLast xs x) > w = xs ++ [x]
  | otherwise = init xs ++ [appendToLast xs x]

appendToLast :: [Text] -> Text -> Text
appendToLast xs x = T.unwords [last xs, x]
