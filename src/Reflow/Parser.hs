module Reflow.Parser where

import Control.Monad (void)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Text.Parsec
import Text.Parsec.Text (Parser)

import Reflow.Types

parseFile :: Text -> [Content]
parseFile t = either (const []) id $ parse parseContent "content" t

parseContent :: Parser [Content]
parseContent = do
    h <- option [] (many header)
    c <- many (quoted <|> codeBlock <|> normal)
    void eof
    return (h <> c)

normal :: Parser Content
normal = Normal <$> singleLine

header :: Parser Content
header = do
    name <- many1 $ noneOf ":\n"
    start <- many1 $ noneOf "\n"
    void newline
    rest <- many headerContinued
    let value = T.pack start <> "\n" <> T.unlines rest
    return $ Header $ T.pack name <> value

headerContinued :: Parser Text
headerContinued = do
    void $ char '\t'
    text <- many1 $ noneOf "\n"
    void newline
    return $ "\t" <> T.pack text <> "\n"

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
    void eol
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
