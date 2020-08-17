module Parser where
-- parse a W3C grammar and build an AST of it

import qualified Text.Parsec as P
import Text.Parsec ((<|>),(<?>))
import Text.ParserCombinators.Parsec (Parser)
import qualified Data.Map as Map
import XPath3EBNF
import W3cData
import Control.Monad
-- import Debug
import Utils

w3c::String->String->Either P.ParseError EBNF
w3c n=P.parse (w3c_ebnf n) "r-site Expr-EBNF parser"

w3c_ebnf::String->Parser EBNF
w3c_ebnf n=do
  wss
  e<-P.many symbol
  P.eof
  return$EBNF n e --(Map.fromList$map (\(Rule n d)->(getName n,d)) e)

symbol::Parser Symbol
symbol=do
  n<-name
  wss
  P.string "::="
  wss
  e<-expr
  return$Rule n e

--------------------------------------------------------
-- exprs::Parser Expr
expr=do
  e<-seqs `P.sepBy` (P.char '|'>>wss)
  case e of
    [o]->return$Expr o
    _->return$Alt e

seqs::Parser Seq
seqs=do
    m<-P.many1 (P.try mods)
    wss
    case m of
      [o]->
        do
          P.char '-'
          wss
          x<-mods
          return(Except o x)
        <|>return(Seq o)
      _->return(Mods m)

--------------------------------------------------------
mods :: Parser Mod
mods=do
  e<-terms<?>"terms"
  wss
  do {P.char '+';wss;return$Many1 e}
    <|> do {P.char '*';wss;return$Many e}
    <|> do {P.char '?';wss;return$Option e}
    <|>do
      P.notFollowedBy (P.try$P.string "::=")
      -- P.notFollowedBy (P.try$P.string "-")
      return$Term e

--------------------------------------------------------
terms::Parser Terms
terms
  =(ref<?>"ref")
  <|>(text<?>"text")
  <|>(ch<?>"character")
  <|>(P.try noneOf<?>"noneOf")
  <|>(oneOf<?>"oneOf")
  <|>(parens<?>"parens")

ref::Parser Terms
ref=do
  n<-name<?>"ref name"
  return$Ref n

text::Parser Terms
text=do
  s<-escStr '"' <|> escStr '\''
  return $ Text s

ch::Parser Terms
ch=do
  c<-code
  return $ Ch c

oneOf::Parser Terms
oneOf=do
  P.char '['
  r<-chars
  P.char ']'
  return $ OneOf r

noneOf::Parser Terms
noneOf=do
  P.char '['
  P.char '^'
  r<-chars
  P.char ']'
  return $ NoneOf r

parens :: Parser Terms
parens=do
  P.char '('
  wss
  e<-expr
  P.char ')'
  wss
  return $Parens e

-----------------------------------------------------------------------------
name::Parser Name
name
  =do
    o<-P.upper<?>"Data name"
    oo<-P.many P.alphaNum
    return $Data (o:oo)
  <|>do
    o<-P.letter<?>"Symbol name"
    oo<-P.many P.alphaNum
    return $Symbol (o:oo)

-----------------------------------------------------------------------------
comment::Parser Char
comment
  =do
    P.string "/*"
    P.manyTill P.anyChar (P.try (P.string "*/"))
    wss
    return '?'
  <|>do -- TODO: make <?...?> a processing instructions instead of a comment
    P.string "<?"
    P.manyTill P.anyChar (P.try (P.string "?>"))
    wss
    return '?'

wss::Parser ()
wss=void(P.many ws)
ws::Parser Char
ws=P.space<|>P.endOfLine<|>P.tab<|>comment

range::Parser o->Parser (Range o)
range o=do
  f<-P.try o
  P.char '-'
  t<-o
  return$Range f t

chars::Parser Chars
chars
  =do {r<-P.try$P.many1 (range code);return$HexSet$Ranges r}
  <|>do {r<-P.try$P.many1 (range (P.noneOf "]"));return$CharSet$Ranges r}
  <|>do {r<-P.try$P.many1 code;return$HexSet$ChSeq r}
  <|>do {r<-P.try$P.many1 (P.noneOf "]");return$CharSet$ChSeq r}

code::Parser Code
code=do
  P.char '#'
  P.char 'x'
  o<-P.many1 P.hexDigit
  return $Code o

---------------------------------------------------------------
escStr::Char -> Parser String
escStr c=do
  P.char c
  chunkStr
  where
    chunkStr::Parser String
    chunkStr=do
      s<-P.many (P.noneOf ['\\',c])
      e<-P.anyChar
      if e==c
      then return s
      else do
        n<-P.anyChar
        cs<-chunkStr
        return $ s++['\\',n]++cs
