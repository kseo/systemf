{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Language.LambdaCalculus.Parser.Common
  ( BoundContext
  , LCParser
  , infoFrom
  , brackets
  , braces
  , parens
  , identifier
  , reserved
  , reservedOp
  , whiteSpace
  , arrow
  , lambda
  , tlambda
  , colon
  , dot
  , star
  , equals
  , comma
  ) where

import Language.LambdaCalculus.AST

import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as P

infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

lcDef :: LanguageDef st
lcDef = emptyDef
  { P.identStart      = letter
  , P.identLetter     = letter <|> char '\''
  , P.reservedNames   = ["true", "false", "if", "then", "else", "All", "Some", "as", "let", "in"]
  , P.reservedOpNames = ["->", ":", ".", "\\"]
  }

lexer       = P.makeTokenParser lcDef
parens      = P.parens lexer
brackets    = P.brackets lexer
braces      = P.braces lexer
identifier  = P.identifier lexer
reserved    = P.reserved lexer
reservedOp  = P.reservedOp lexer
whiteSpace  = P.whiteSpace lexer
tlambda     = P.symbol lexer "/\\"
lambda      = P.symbol lexer "\\"
arrow       = P.symbol lexer "->"
star        = P.symbol lexer "*"
equals      = P.symbol lexer "="
comma       = P.comma lexer
dot         = P.dot lexer
colon       = P.colon lexer

type BoundContext = [String]
type LCParser a = Parsec String BoundContext a
