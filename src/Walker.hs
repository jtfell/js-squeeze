{-# LANGUAGE DeriveGeneric #-}

module Walker where

import AST
import Data.Text (Text)

instance Functor Program where
    fmap (Program s) = Program (fmap s)
 
instance Functor Identifier where
    fmap (Identifier id) = Identifier (fmap id)

instance Functor LitType where
    fmap (StringLit t) = StringLit (fmap t)
    fmap TrueLit = TrueLit
    fmap FalseLit = FalseLit
    fmap (NumLit n) = NumLit (fmap n)
    fmap Regex = Regex

instance Functor Literal where
    fmap = Literal fmap

instance Functor Function where
    fmap (Function id lam) = Function (fmap id) (fmap lam)

instance Functor Lambda where
    fmap (Lambda patts blk) = Lambda (fmap patts) (fmap blk)

instance Functor Block where
    fmap = Block fmap

instance Functor Statement where
    fmap EmptyStatement = EmptyStatement
    fmap (BlockStatement blk) = BlockStatement (fmap blk)
    fmap (ExpressionStatement expr) = ExpressionStatement (fmap expr)
    fmap (IfStatement expr st mst) = IfStatement (fmap expr) (fmap st) (fmap mst)
    fmap (ReturnStatement mexp) = ReturnStatement (fmap mexp)
    fmap DebuggerStatement = DebuggerStatement
    fmap (FunctionDeclaration fn) = FunctionDeclaration (fmap fn)
    fmap (VariableDeclaration var) = VariableDeclaration (fmap var)

instance Functor VariableDecl where
    fmap = VariableDecl fmap

instance Functor VariableDeclarator where
    fmap (VariableDeclarator patt exp) = VariableDeclarator (fmap patt) (fmap exp)

instance Functor MemberProp where
    fmap (MemId id) = fmap id
    fmap (MemExpr exp) = fmap exp

instance Functor Expression where
    fmap ThisExpression = ThisExpression
    fmap (ArrayExpression exprs) = ArrayExpression (fmap exprs)
    fmap (FunctionExpression fn) = FunctionExpression (fmap fn)
    fmap (BinaryExpression op a b) = BinaryExpression (fmap op) (fmap a) (fmap b)
    fmap (AssignmentExpression op a b) = AssignmentExpression (fmap op) (fmap a) (fmap b)
    fmap (CallExpression fn args) = CallExpression (fmap fn) (fmap args)
    fmap (MemberExpression expr props) = MemberExpression (fmap expr) (fmap props)
    fmap (IdentifierExpression id) = IdentifierExpression (fmap id)
    fmap (LiteralExpression lit) = LiteralExpression (fmap lit)

instance Functor Pattern where
    fmap (IdentifierPattern id) = IdentifierPattern (fmap id)
    fmap (ExprPattern expr) = ExprPattern (fmap expr)

instance Functor AssignmentOperator where
    fmap Assign = Assign
    fmap PlusAssign = PlusAssign

instance Functor BinaryOperator where
    fmap Plus = Plus
    fmap Minus = Minus
