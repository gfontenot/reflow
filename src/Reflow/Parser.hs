module Reflow.Parser where

import Control.Monad (void)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import qualified Data.Text as T
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
    return $ h <> c

normal :: Parser Content
normal = Normal <$> singleLine

header :: Parser Content
header = do
    n <- headerName
    s <- singleLine
    r <- many headerContinued
    let v = s <> T.unlines r
    return $ Header $ n <> v

headerName :: Parser Text
headerName = do
    n <- manyTill (noneOf "\n") (lookAhead $ string ":")
    s <- string ":"
    return $ pack $ n <> s

headerContinued :: Parser Text
headerContinued = do
    void tab
    text <- singleLine
    void eol
    return $ "\t" <> text <> "\n"

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
