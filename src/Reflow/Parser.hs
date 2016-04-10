module Reflow.Parser where

import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Text.Parsec
import Text.Parsec.Text (Parser)

import Reflow.Types

parseFile :: Text -> [Content]
parseFile t = either (const []) id $ parse parseContent "content" t

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
singleLine = pack <$> manyTill anyChar (try eol)

quoteChar :: Parser Text
quoteChar = pack <$> (string ">")

codeBlockChar :: Parser Text
codeBlockChar = pack <$> (string "```")

codeBlockContents :: Parser Text
codeBlockContents = pack <$> manyTill anyChar (lookAhead codeBlockChar)

eol :: Parser String
eol = try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"
