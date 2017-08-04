{-# LANGUAGE OverloadedStrings #-}

module CodeGen where

import Data.Text as T

import AST

class Gen a where
    gen :: a -> T.Text

instance Gen Identifier where
  gen (Identifier name) = name

instance Gen Literal where
  gen (Literal t) = gen t

instance Gen LitType where
  gen (StringLit text) = text
  gen TrueLit = "true"
  gen FalseLit = "false"
  gen (NumLit num) = T.pack $ show num

instance Gen Program where
  gen (Program statements) = T.intercalate ";" $ fmap gen statements

instance Gen Statement where
  gen (ExpressionStatement s) = gen s
  gen (VariableDeclaration s) = gen s

instance Gen VariableDecl where
  gen (VariableDecl vars) = T.intercalate "," $ fmap gen vars

instance Gen VariableDeclarator where
  gen (VariableDeclarator p Nothing) = mappend "var " (gen p)
  gen (VariableDeclarator p (Just e)) = T.concat ["var ", gen p, "=", gen e]

instance Gen Pattern where
  gen (ExprPattern e) = gen e
  gen (IdentifierPattern i) = gen i

instance Gen Expression where
  gen (LiteralExpression e) = gen e
  gen (IdentifierExpression e) = gen e
  gen (AssignmentExpression op e1 e2) = T.concat [gen e1, gen op, gen e2]
  gen (BinaryExpression op e1 e2) = T.concat [gen e1, gen op, gen e2]

instance Gen AssignmentOperator where
  gen Assign = "="
  gen PlusAssign = "+="
  gen MinusAssign = "-="
  gen MultAssign = "*="
  gen DivAssign = "/="
  gen ModAssign = "%="

instance Gen BinaryOperator where
  gen Plus = "+"
  gen Minus = "-"
  gen Times = "*"
  gen Div = "/"
  gen Equal = "=="
