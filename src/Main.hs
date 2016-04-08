{-# LANGUAGE OverloadedStrings #-}

import Options.Applicative (execParser)
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Hfold.CLI

main :: IO ()
main = execParser opts >>= run

run :: Config -> IO ()
run (Config w p) = do
    c <- contentsOrSTDIN p
    T.putStrLn $ T.stripEnd $ T.unlines $ wrapLine w =<< T.lines c

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
