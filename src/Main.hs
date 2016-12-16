import Options.Applicative (execParser)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Reflow.Types
import Reflow.CLI
import Reflow.Parser

main :: IO ()
main = execParser opts >>= run

run :: Config -> IO ()
run c = T.putStrLn . wrap c . parseFile =<< contentsOrSTDIN (path c)

contentsOrSTDIN :: FilePath -> IO Text
contentsOrSTDIN "-" = T.getContents
contentsOrSTDIN p = T.readFile p

wrap :: Config -> [Content] -> Text
wrap c = T.stripEnd . T.unlines . concatMap (wrapContent c)

wrapContent :: Config -> Content -> [Text]
wrapContent _ (Blank) = [""]
wrapContent _ (CodeBlock t) = [t]
wrapContent _ (PGPBlock t) = [t]
wrapContent c (Quoted t) = mapPrepend "> " $ wrapContent (lessWidth 2 c) t
wrapContent c (Normal i t) = mapPrepend (emptyText i) $ wrapLine (width c - i) t
wrapContent c (Header t)
    | ignoreHeaders c = [t]
    | otherwise = wrapLine (width c) t

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

mapPrepend :: Text -> [Text] -> [Text]
mapPrepend t ts = map (mappend t) ts

emptyText :: Int -> Text
emptyText i = T.pack $ replicate i ' '
