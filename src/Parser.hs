{-# LANGUAGE OverloadedStrings #-}

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
-- Functions
--
function :: Parser Function
function = do
    reserved "function"
    id <- optionMaybe ident
    lam <- lambda
    return $ Function id lam

lambda :: Parser Lambda
lambda = do
    params <- parens $ commaSep pat
    body <- block
    return $ Lambda params body

block :: Parser Block
block = Block <$> braces (statement `sepEndBy` reservedOp ";")

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
-- Variables
--
variableDeclarator :: Parser VariableDeclarator
variableDeclarator = do
    var <- pat
    ex <- optionMaybe (reservedOp "=" *> expr)
    return $ VariableDeclarator var ex

variableDecl :: Parser VariableDecl
variableDecl = do
    reserved "var"
    decls <- commaSep variableDeclarator
    return $ VariableDecl decls

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
factor = try functionExpr
     <|> try callExpr
     <|> try primaryExpr
     <|> try memberExpr
     <|> parens expr

primaryExpr = try identifierExpr
     <|> try literalExpr
     <|> try thisExpr
 
memberProp :: Parser MemberProp
memberProp = MemId <$> (reservedOp "." *> ident)

memberExpr :: Parser Expression
memberExpr = do
    prim <- primaryExpr
    parts <- many memberProp
    return $ MemberExpression prim parts

thisExpr :: Parser Expression
thisExpr = reserved "this" *> return ThisExpression

functionExpr :: Parser Expression
functionExpr = FunctionExpression <$> function

callExpr :: Parser Expression
callExpr = do
    func <- memberExpr
    params <- parens $ expr `sepBy` reservedOp ","
    return $ CallExpression func params

literalExpr :: Parser Expression
literalExpr = LiteralExpression . Literal <$> litType

identifierExpr :: Parser Expression
identifierExpr = IdentifierExpression <$> ident

--
-- Statements
--
exprStatement :: Parser Statement
exprStatement = ExpressionStatement <$> expr

funcStatement :: Parser Statement
funcStatement = FunctionDeclaration <$> function

variableDeclStatement :: Parser Statement
variableDeclStatement = VariableDeclaration <$> variableDecl

returnStatement :: Parser Statement
returnStatement = ReturnStatement <$> (reserved "return" *> optionMaybe expr)

statement :: Parser Statement
statement = try variableDeclStatement
    <|> try funcStatement
    <|> try returnStatement
    <|> try exprStatement

--
-- Toplevel
--
program :: Parser Program
program = Program <$> statement `sepEndBy` reservedOp ";"

contents :: Parser a -> Parser a
contents p = Tok.whiteSpace lexer *> p <* eof
