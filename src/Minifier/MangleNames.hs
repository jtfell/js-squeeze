{-# LANGUAGE OverloadedStrings #-}

module Minifier.MangleNames (mangleNames) where

import AST
import Data.Text as T
import Data.Map as M

--
-- Mangle names
--
-- eg. var longName = 'good';console.log(longName); => var a = 'good';console.log(a);
--

-- Keep track of all the vars in a scope so we can effectively mangle them
data VarScope = VarScope {
    nameMapping :: M.Map Text Text
  , val :: Text
  } deriving (Show)

-- 
-- Generate a mapping of all the vars declared in a list of statements
--
getVarDecNames :: VariableDecl -> [Text]
getVarDecNames (VariableDecl decls) = fmap getVarDecName decls 
  where getVarDecName (VariableDeclarator (IdentifierPattern (Identifier id)) _) = id

addNewMapping :: VarScope -> Text -> VarScope
addNewMapping scope id = scope {
    nameMapping = insertIfNotExists id (val scope) (nameMapping scope)
  , val = mappend (val scope) "a" -- TODO: go up the alphabet
  }

insertIfNotExists = M.insertWith (\new old -> old)

addToScope :: VarScope -> Statement -> VarScope
addToScope scope (VariableDeclaration varDec) = addMultipleMappings scope varDec
addToScope scope _ = scope

addMultipleMappings scope decls = Prelude.foldl addNewMapping scope $ getVarDecNames decls

buildUpScope :: [Statement] -> VarScope
buildUpScope = Prelude.foldl addToScope initial
  where initial = VarScope { nameMapping = M.empty, val = "z" }

--
-- Rename all usages of each mapping in the scope
--
renameStatement :: VarScope -> Statement -> Statement
renameStatement scope (VariableDeclaration (VariableDecl decls)) = VariableDeclaration (VariableDecl (remapDecl scope decls))
renameStatement _ s = s

remapDecl :: VarScope -> [VariableDeclarator] -> [VariableDeclarator]
remapDecl scope = fmap (\ (VariableDeclarator patt exp) -> VariableDeclarator (remapPatt scope patt) exp)

remapPatt :: VarScope -> Pattern -> Pattern
remapPatt scope (IdentifierPattern (Identifier id)) = IdentifierPattern (Identifier (M.findWithDefault "uh oh" id (nameMapping scope)))
remapPatt _ p = p

mangleNames :: [Statement] -> [Statement]
-- mangleNames ss = fmap (renameStatement (buildUpScope ss)) ss
--
-- TODO: This is getting way too messy. Need a generalised tree walker or something
mangleNames = id
