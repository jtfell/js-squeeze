{-# LANGUAGE OverloadedStrings #-}

module Minifier.MangleNames (mangleNames) where

import AST
import Data.Text as T
import Data.Map as M

import Control.Lens

--
-- Mangle names
--
-- eg. var longName = 'good';console.log(longName); => var a = 'good';console.log(a);
--

-- Keep track of all the vars in a scope so we can effectively mangle them
type VarScope = M.Map Text Text

-- 
-- Generate a mapping of all the vars declared in a list of statements
--
getVarDecNames :: VariableDecl -> [Text]
getVarDecNames (VariableDecl decls) = fmap getVarDecName decls 

getVarDecName :: VariableDeclarator -> Text
getVarDecName (VariableDeclarator (IdentifierPattern (Identifier id)) _) = id

-- TODO: use lenses for this somehow
-- getVarDecNames :: VariableDecl -> [Text]
-- getVarDecNames = view (var_decls . traverse . var_patt . patt_id . id_id)

addNewMapping :: VarScope -> Text -> VarScope
addNewMapping scope id = insertIfNotExists id "a" scope
  -- TODO: go up the alphabet (Use state monad?)
  where insertIfNotExists = M.insertWith (\new old -> old)

addToScope :: VarScope -> Statement -> VarScope
addToScope scope (VariableDeclaration varDec) = addMultipleMappings scope varDec
addToScope scope _ = scope

addMultipleMappings scope decls = Prelude.foldl addNewMapping scope $ getVarDecNames decls

buildUpScope :: [Statement] -> VarScope
buildUpScope = Prelude.foldl addToScope M.empty

--
-- Rename all usages of each mapping in the scope
--
remapStatement :: VarScope -> Statement -> Statement
remapStatement scope (VariableDeclaration (VariableDecl decls)) = VariableDeclaration (VariableDecl (remapDecl scope decls))
remapStatement scope (ExpressionStatement exp) = ExpressionStatement (remapExp scope exp)
remapStatement scope (ReturnStatement Nothing) = ReturnStatement Nothing
remapStatement scope (ReturnStatement (Just exp)) = ReturnStatement (Just (remapExp scope exp))
remapStatement _ s = s


remapExp :: VarScope -> Expression -> Expression
remapExp scope (SequenceExpression seq) = SequenceExpression (fmap (remapExp scope) seq)
remapExp scope (UnaryExpression op b exp) = UnaryExpression op b (remapExp scope exp)
remapExp scope (BinaryExpression op e1 e2) = BinaryExpression op (remapExp scope e1)(remapExp scope e2)
remapExp scope (AssignmentExpression op e1 e2) = AssignmentExpression op (remapExp scope e1)(remapExp scope e2)
remapExp scope (IdentifierExpression id) = IdentifierExpression (remapIdent scope id)
remapExp _ exp = exp


remapDecl :: VarScope -> [VariableDeclarator] -> [VariableDeclarator]
remapDecl scope = fmap (\ (VariableDeclarator patt exp) -> VariableDeclarator (remapPatt scope patt) exp)


remapPatt :: VarScope -> Pattern -> Pattern
remapPatt scope (IdentifierPattern id) = IdentifierPattern (remapIdent scope id)
remapPatt _ p = p


remapIdent :: VarScope -> Identifier -> Identifier
remapIdent scope (Identifier id) = Identifier (M.findWithDefault "uh oh" id scope)

-- Build up mapping of old var names to new short ones, then map over
-- each statement in the list and make the substitutions
mangleNames :: [Statement] -> [Statement]
mangleNames ss = fmap (remapStatement (buildUpScope ss)) ss
