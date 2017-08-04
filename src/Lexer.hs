module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = [
      -- Binary ops
      "+","*","-",
      "==","===","!==", "!=",
      ">", ">=", "<", "<=",
      "%", ">>", "<<", ">>>", "<<<",
      "||","&&","|","&",
      "instanceof", "in",
      ";",

      -- Unary ops
      "!","~","typeof","void","delete",
      
      -- Assignment ops
      "=", "+=", "-=", "*=", "/=", "%=",
      ">>=", "<<=", "<<<=", ">>>=", "|=", "&=",
      
      -- Update ops
      "++", "--"]
    names = [
      "var",
      "function",
      "return",

      "true",
      "false",

      "switch",
      "case",
      "break",
      "default",

      "if",
      "else",

      "while",
      "do",
      "for",
      "of",

      "new",

      "throw",
      "try",
      "catch",

      "null",
      "undefined"]
    style = emptyDef {
               Tok.commentLine = "//"
             , Tok.reservedOpNames = ops
             , Tok.reservedNames = names
             }

float :: Parser Double
float = do
    num <- Tok.naturalOrFloat lexer
    case num of
        (Left int) -> return (fromIntegral int)
        (Right fl) -> return fl

stringLiteral :: Parser String
stringLiteral = Tok.stringLiteral lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

colon :: Parser String
colon = Tok.colon lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer
