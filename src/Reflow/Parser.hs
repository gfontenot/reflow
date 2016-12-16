module Reflow.Parser where

import Control.Monad (void)
import Data.Monoid ((<>))
import Data.Text (Text, pack, singleton)
import Text.Parsec
import Text.Parsec.Text (Parser)

import Reflow.Types

parserError :: Text -> ParseError -> Content
parserError input e = Normal 0
    $ input
    <> "\n\n\n"
    <> "Warning: reflow parser failed\n"
    <> (pack $ show e)

parseFile :: Text -> [Content]
parseFile t = either (return . parserError t) id
    $ parse parseContent "content" t

parseContent :: Parser [Content]
parseContent = do
    h <- option [] (try $ many header)
    c <- many (blank <|> quoted <|> try codeBlock <|> try pgpBlock <|> normal)
    void eof
    return $ h <> c

normal :: Parser Content
normal = do
    ws <- many (char ' ')
    l <- singleLine
    return $ Normal (length ws) l

blank :: Parser Content
blank = do
    _ <- try $ trailingWhitespace >> eol
    return Blank

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
codeBlock = fmap CodeBlock
    $ blockContents codeBlockChar codeBlockContents codeBlockChar

pgpBlock :: Parser Content
pgpBlock = fmap PGPBlock
    $ blockContents pgpBlockStart pgpBlockContents pgpBlockEnd

blockContents :: Parser Text -> Parser Text -> Parser Text -> Parser Text
blockContents start contents end = do
    s <- start
    c <- contents
    e <- end
    void $ trailingWhitespace >> eol
    return $ s <> c <> e

singleLine :: Parser Text
singleLine = pack <$> manyTill anyChar (try $ trailingWhitespace >> eol)

quotePrefix :: Parser Text
quotePrefix = fmap pack $ mappend
    <$> string ">"
    <*> (many $ char ' ')

codeBlockChar :: Parser Text
codeBlockChar = pack <$> (string "```")

codeBlockContents :: Parser Text
codeBlockContents = pack <$> manyTill anyChar (lookAhead codeBlockChar)

pgpBlockStart :: Parser Text
pgpBlockStart = pack <$> string "[-- Begin signature information --]"

pgpBlockEnd :: Parser Text
pgpBlockEnd = pack <$> string "[-- End signature information --]"

pgpBlockContents :: Parser Text
pgpBlockContents = pack <$> manyTill anyChar (lookAhead pgpBlockEnd)

trailingWhitespace :: Parser Text
trailingWhitespace = pack <$> manyTill (char ' ') (lookAhead eol)

eol :: Parser Text
eol = singleton <$> endOfLine
