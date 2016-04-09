module Hfold.Parser where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Text.ParserCombinators.Parsec

import Hfold.Types

parseFile :: Text -> [Content]
parseFile t = case parse parseContent "content" (T.unpack t) of
    Left err -> []
    Right xs -> xs

parseContent :: Parser [Content]
parseContent = many (quoted <|> codeBlock <|> normal) <* eof

normal :: Parser Content
normal = Normal <$> singleLine

quoted :: Parser Content
quoted = do
    q <- quoteChar
    l <- singleLine
    return $ Quoted (q <> l)

codeBlock :: Parser Content
codeBlock = do
    s <- codeBlockChar
    c <- codeBlockContents
    e <- codeBlockChar
    eol
    return $ CodeBlock $ s <> c <> e

singleLine :: Parser Text
singleLine = T.pack <$> manyTill anyChar (try eol)

quoteChar :: Parser Text
quoteChar = T.pack <$> (string ">")

codeBlockChar :: Parser Text
codeBlockChar = T.pack <$> (string "```")

codeBlockContents :: Parser Text
codeBlockContents = T.pack <$> manyTill anyChar (lookAhead codeBlockChar)

eol :: Parser String
eol = try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"
