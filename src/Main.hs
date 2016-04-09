{-# LANGUAGE OverloadedStrings #-}

import Options.Applicative (execParser)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Hfold.Types
import Hfold.CLI
import Hfold.Parser

main :: IO ()
main = execParser opts >>= run

run :: Config -> IO ()
run (Config w p) = T.putStrLn . wrap w . parseFile =<< contentsOrSTDIN p

contentsOrSTDIN :: FilePath -> IO Text
contentsOrSTDIN "-" = T.getContents
contentsOrSTDIN p = T.readFile p

wrap :: Int -> [Content] -> Text
wrap w = T.stripEnd . T.unlines . concatMap (wrapContent w)

wrapContent :: Int -> Content -> [Text]
wrapContent _ (CodeBlock t) = [t]
wrapContent _ (Quoted t) = [t]
wrapContent w (Normal t) = wrapLine w t

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
