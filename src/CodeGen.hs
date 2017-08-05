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
  gen (StringLit text) = T.concat ["'", text, "'"]
  gen TrueLit = "!0"
  gen FalseLit = "!1"
  gen (NumLit num) = T.pack $ show num

instance Gen Program where
  gen (Program statements) = mappend (T.intercalate ";" $ fmap gen statements) ";"

instance Gen Statement where
  gen (ExpressionStatement s) = gen s
  gen (ReturnStatement Nothing) = "return"
  gen (ReturnStatement (Just s)) = mappend "return " $ gen s
  gen (VariableDeclaration s) = gen s
  gen (FunctionDeclaration s) = gen s

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
  gen (CallExpression id args) = T.concat [gen id, "(", T.intercalate "," $ fmap gen args, ")"]
  gen (FunctionExpression func) = gen func
  gen (MemberExpression mem props) = T.concat [gen mem, T.concat $ fmap gen props]

instance Gen MemberProp where
  gen (MemId id) = mappend "." $ gen id

instance Gen Function where
  gen (Function Nothing lam) = T.concat ["function ", gen lam]
  gen (Function (Just id) lam) = T.concat ["function ", gen id, gen lam]

instance Gen Lambda where
  gen (Lambda patt blk) = T.concat ["(", T.intercalate "," $ fmap gen patt, ")", gen blk]

instance Gen Block where
  gen (Block statements) = T.concat ["{", T.intercalate ";" $ fmap gen statements, ";}"]

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
