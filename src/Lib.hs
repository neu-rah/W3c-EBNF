{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib(
  Parser.w3c
 ,Parser.w3c_ebnf
 ,Compiler.run
 ,Compiler.compile
 ,XPath3EBNF.xpath3_ebnf
 ,module Lib
 ) where

import Text.Parsec
import Text.ParserCombinators.Parsec (Parser)
import qualified Data.Map as Map
import Data.Monoid hiding (Alt)
import Text.Heredoc
import Language.LBNF

import W3cData
import Parser
import Compiler
import XPath3EBNF
import Utils

import Control.Monad.State.Lazy

w3cGrammar=[lbnf|
token UIdent (upper (letter | digit | '_')*) ;
|]

Right xpath_ast=w3c "XPath" xpath3_ebnf
Right (xpath_map,xpath_parser)=compile $optimize xpath_ast

testEbnf=[here|
Digits   ::= [0-9]+
|]

(Right testAst)=w3c "Digits" testEbnf
testParser=cc$compile $optimize testAst
