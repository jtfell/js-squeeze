{-# LANGUAGE OverloadedStrings #-}

--
-- Leaving out:
--
--   * functions
--   * arrays
--   * objects
--   * if/else
--   * loops (incl break, continue etc)
--

module Parser where

import Data.Text

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import AST

ident :: Parser Identifier
ident = Identifier <$> fmap pack identifier

pat :: Parser Pattern
pat = try id <|> try ex
    where
        id = IdentifierPattern <$> ident
        ex = ExprPattern <$> expr

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
binary s f = Ex.Infix $ reservedOp s >> return f -- (BinaryExpression f)

table = [
    -- Assignments
    [binary "="  (AssignmentExpression Assign) Ex.AssocLeft,
     binary "+=" (AssignmentExpression PlusAssign) Ex.AssocLeft,
     binary "-=" (AssignmentExpression MinusAssign) Ex.AssocLeft,
     binary "*=" (AssignmentExpression MultAssign) Ex.AssocLeft,
     binary "/=" (AssignmentExpression DivAssign) Ex.AssocLeft,
     binary "%=" (AssignmentExpression ModAssign) Ex.AssocLeft]

    -- Bin ops
   ,[binary "*" (BinaryExpression Times) Ex.AssocLeft,
     binary "/" (BinaryExpression Div) Ex.AssocLeft]
   ,[binary "+" (BinaryExpression Plus) Ex.AssocLeft,
     binary "-" (BinaryExpression Minus) Ex.AssocLeft]]

expr :: Parser Expression
expr = Ex.buildExpressionParser table factor

factor :: Parser Expression
factor = try literalExpr
     <|> try identifierExpr
     <|> parens expr

literalExpr :: Parser Expression
literalExpr = LiteralExpression . Literal <$> litType

identifierExpr :: Parser Expression
identifierExpr = IdentifierExpression <$> ident

--
-- Statements
--
exprStatement :: Parser Statement
exprStatement = ExpressionStatement <$> expr

variableDeclarator :: Parser VariableDeclarator
variableDeclarator = do
    var <- pat
    ex <- optionMaybe (reservedOp "=" *> expr)
    return $ VariableDeclarator var ex

variableDecl :: Parser VariableDecl
variableDecl = do
    reserved "var"
    decls <- variableDeclarator `sepBy1` reservedOp ","
    return $ VariableDecl decls

variableDeclStatement :: Parser Statement
variableDeclStatement = VariableDeclaration <$> variableDecl

statement :: Parser Statement
statement = try exprStatement
    <|> try variableDeclStatement

--
-- Toplevel
--
program :: Parser Program
program = Program <$> statement `endBy` reservedOp ";"

contents :: Parser a -> Parser a
contents p = Tok.whiteSpace lexer *> p <* eof
