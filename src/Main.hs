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
run c@(Config _ _ p) = T.putStrLn . wrap c . parseFile =<< contentsOrSTDIN p

contentsOrSTDIN :: FilePath -> IO Text
contentsOrSTDIN "-" = T.getContents
contentsOrSTDIN p = T.readFile p

wrap :: Config -> [Content] -> Text
wrap c = T.stripEnd . T.unlines . concatMap (wrapContent c)

wrapContent :: Config -> Content -> [Text]
wrapContent _              (CodeBlock t) = [t]
wrapContent _              (Quoted t) = [t]
wrapContent (Config w _ _) (Normal t) = wrapLine w t
wrapContent (Config w i _) (Header t)
    | i = [t]
    | otherwise = wrapLine w t

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
