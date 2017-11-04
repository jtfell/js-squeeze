{-# LANGUAGE OverloadedStrings #-}

--
-- Mangle names
--
-- eg. var longName = 'good';console.log(longName); => var a = 'good';console.log(a);
--
-- Process:
--
-- 1. Generate a list of all the identifiers in the program
-- 2. Create a mapping from each unique identifiers to a shortened name (ascending the alphabet)
-- 3. Transform each construct in the program, substituting each identifier as per the mapping
--

module Minifier.MangleNames (mangleNames) where

import AST
import Data.Text as T hiding (concatMap, length, take, zip, foldl)
import Data.Map as M hiding (foldl)

import Control.Monad (replicateM)

-- Mapping of all the identifiers in a scope so we can effectively mangle them
type VarScope = M.Map Text Text

-- 
-- 1. Generate a list of all the identifiers in the program
--
getVarDecls :: VariableDecl -> [Text]
getVarDecls (VariableDecl decls) = concatMap getVarDecl decls

getVarDecl :: VariableDeclarator -> [Text]
getVarDecl (VariableDeclarator patt exp) = getPatt patt

getStatement :: Statement -> [Text]
getStatement (VariableDeclaration varDec) = getVarDecls varDec
getStatement (BlockStatement blk) = getBlock blk
getStatement (FunctionDeclaration fn) = getFnDecl fn
getStatement _ = []

getPatt :: Pattern -> [Text]
getPatt (IdentifierPattern id) = getIdentifier id

getLambda :: Lambda -> [Text]
getLambda (Lambda patt blk) = mappend (concatMap getPatt patt) (getBlock blk)

getFnDecl :: Function -> [Text]
getFnDecl (Function Nothing lam) = getLambda lam
getFnDecl (Function (Just id) lam) = mappend (getIdentifier id) (getLambda lam)

getIdentifier :: Identifier -> [Text]
getIdentifier (Identifier txt) = [txt]

getBlock :: Block -> [Text]
getBlock (Block ss) = concatMap getStatement ss

--
-- 2. Create a mapping from each unique identifiers to a shortened name (ascending the alphabet)
--
identList = fmap T.pack $ [1..] >>= (`replicateM` "abcdefghijklmnopqrstuvwxyz") 

addMapping :: VarScope -> (Text, Text) -> VarScope
addMapping scope (txt, id) = insertIfNotExists txt id scope
  where insertIfNotExists = M.insertWith (\new old -> old)

constructScope :: [Statement] -> VarScope
constructScope ss = foldl addMapping M.empty identPairs
  where identPairs = zip idents (take (length idents) identList)
        idents = concatMap getStatement ss

--
-- 3. Transform each construct in the program, substituting each identifier as per the mapping
--
remapStatement :: VarScope -> Statement -> Statement
remapStatement scope (BlockStatement (Block blk)) = BlockStatement (Block (fmap (remapStatement scope) blk))
remapStatement scope (ExpressionStatement exp) = ExpressionStatement (remapExp scope exp)
remapStatement scope (ReturnStatement Nothing) = ReturnStatement Nothing
remapStatement scope (ReturnStatement (Just exp)) = ReturnStatement (Just (remapExp scope exp))
remapStatement scope (FunctionDeclaration fn) = FunctionDeclaration (remapFn scope fn)
remapStatement scope (VariableDeclaration (VariableDecl decls)) = VariableDeclaration (VariableDecl (remapDecls scope decls))
remapStatement _ s = s


remapExp :: VarScope -> Expression -> Expression
remapExp scope (FunctionExpression fn) = FunctionExpression (remapFn scope fn)
remapExp scope (SequenceExpression seq) = SequenceExpression (fmap (remapExp scope) seq)
remapExp scope (UnaryExpression op b exp) = UnaryExpression op b (remapExp scope exp)
remapExp scope (BinaryExpression op e1 e2) = BinaryExpression op (remapExp scope e1)(remapExp scope e2)
remapExp scope (AssignmentExpression op e1 e2) = AssignmentExpression op (remapExp scope e1)(remapExp scope e2)
remapExp scope (LogicalExpression op e1 e2) = LogicalExpression op (remapExp scope e1)(remapExp scope e2)
remapExp scope (CallExpression fnExp params) = CallExpression (remapExp scope fnExp) (fmap (remapExp scope) params)
remapExp scope (MemberExpression exp props) = MemberExpression (remapExp scope exp) props
remapExp scope (IdentifierExpression id) = IdentifierExpression (remapIdent scope id)
remapExp _ exp = exp


remapLambda :: VarScope -> Lambda -> Lambda
remapLambda scope (Lambda p (Block ss)) = Lambda (fmap (remapPatt scope) p) (Block (fmap (remapStatement scope) ss))


remapFn :: VarScope -> Function -> Function
remapFn scope (Function Nothing lam) = Function Nothing (remapLambda scope lam)
remapFn scope (Function (Just id) lam) = Function (Just (remapIdent scope id)) (remapLambda scope lam)


remapDecl :: VarScope -> VariableDeclarator -> VariableDeclarator
remapDecl scope (VariableDeclarator patt (Just exp)) = VariableDeclarator (remapPatt scope patt) (Just (remapExp scope exp))
remapDecl scope (VariableDeclarator patt Nothing) = VariableDeclarator (remapPatt scope patt) Nothing


remapDecls :: VarScope -> [VariableDeclarator] -> [VariableDeclarator]
remapDecls scope = fmap (remapDecl scope)


remapPatt :: VarScope -> Pattern -> Pattern
remapPatt scope (IdentifierPattern id) = IdentifierPattern (remapIdent scope id)
remapPatt scope (ExprPattern exp) = ExprPattern (remapExp scope exp)


-- TODO: Throw a runtime error when identifier not found in scope
remapIdent :: VarScope -> Identifier -> Identifier
remapIdent scope (Identifier id) = Identifier (M.findWithDefault "broken" id scope)

-- Build up mapping of old var names to new short ones, then map over
-- each statement in the list and make the substitutions
mangleNames :: [Statement] -> [Statement]
mangleNames ss = fmap (remapStatement (constructScope ss)) ss
