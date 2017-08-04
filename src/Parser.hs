{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Text

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import AST

--
-- Literals
--
stringLit :: Parser LitType
stringLit = StringLit <$> fmap pack stringLiteral

boolLit :: Parser LitType
boolLit = true <|> false
    where true = reserved "true" *> pure TrueLit
          false = reserved "false" *> pure FalseLit

numLit :: Parser LitType
numLit = NumLit <$> float

litType :: Parser LitType
litType = stringLit <|> boolLit <|> numLit

--
-- Expressions
--
binary s f = Ex.Infix $ reservedOp s >> return (BinaryExpression f)

table = [
    [binary "*" Times Ex.AssocLeft,
     binary "/" Div Ex.AssocLeft]
   ,[binary "+" Plus Ex.AssocLeft,
     binary "-" Minus Ex.AssocLeft]]

expr :: Parser Expression
expr = Ex.buildExpressionParser table factor

factor :: Parser Expression
factor = try literalExpr
     <|> parens expr

literalExpr :: Parser Expression
literalExpr = LiteralExpression . Literal <$> litType

--
-- Statements
--
exprStatement :: Parser Statement
exprStatement = ExpressionStatement <$> expr

statement :: Parser Statement
statement = try exprStatement

--
-- Toplevel
--
program :: Parser Program
program = Program <$> statement `endBy` reservedOp ";"

contents :: Parser a -> Parser a
contents p = Tok.whiteSpace lexer *> p <* eof

-- variable :: Parser Expr
-- variable = do
--   var <- identifier
--   return $ Var var
-- 
-- function :: Parser Expr
-- function = do
--   reserved "def"
--   name <- identifier
--   args <- parens $ many variable
--   body <- expr
--   return $ Function name args body
-- 
-- extern :: Parser Expr
-- extern = do
--   reserved "extern"
--   name <- identifier
--   args <- parens $ many variable
--   return $ Extern name args
-- 
-- call :: Parser Expr
-- call = do
--   name <- identifier
--   args <- parens $ commaSep expr
--   return $ Call name args
-- 
-- factor :: Parser Expr
-- factor = try floating
--       <|> try int
--       <|> try extern
--       <|> try function
--       <|> try call
--       <|> variable
--       <|> parens expr
-- 
-- defn :: Parser Expr
-- defn = try extern
--     <|> try function
--     <|> expr
