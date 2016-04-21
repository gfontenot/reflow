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
    h <- option [] (try $ many header)
    c <- many (quoted <|> try codeBlock <|> normal)
    void eof
    return $ h <> c

normal :: Parser Content
normal = Normal <$> singleLine

header :: Parser Content
header = do
    n <- headerName
    s <- singleLine
    r <- many headerContinued
    let v = s <> mconcat r
    return $ Header $ n <> v

headerName :: Parser Text
headerName = fmap pack $ mappend
    <$> many1 allowedHeaderChars
    <*> string ":"

allowedHeaderChars :: Parser Char
allowedHeaderChars = alphaNum <|> char '-'

headerContinued :: Parser Text
headerContinued = do
    void tab
    text <- singleLine
    return $ "\n\t" <> text

quoted :: Parser Content
quoted = Quoted <$> (quotePrefix *> (quoted <|> normal))

codeBlock :: Parser Content
codeBlock = do
    s <- codeBlockChar
    c <- codeBlockContents
    e <- codeBlockChar
    void eol
    return $ CodeBlock $ s <> c <> e

singleLine :: Parser Text
singleLine = pack <$> manyTill anyChar (try eol)

quotePrefix :: Parser Text
quotePrefix = fmap pack $ mappend
    <$> string ">"
    <*> (many $ char ' ')

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
