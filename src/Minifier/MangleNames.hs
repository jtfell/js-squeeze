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
    nameMapping :: M.Map [Text] [Text]
  , allVars :: [Text]
  } deriving (Show)

getAllNames :: [Statement] -> [Text]
getAllNames [] = []
getAllNames (VariableDeclaration varDec:xs) = mappend (getVarDecNames varDec) (getAllNames xs)
getAllNames (s:ss) = getAllNames ss

getVarDecNames :: VariableDecl -> [Text]
getVarDecNames (VariableDecl decls) = fmap getVarDecNames' decls 

getVarDecNames' (VariableDeclarator (IdentifierPattern (Identifier id)) _) = id

mangleNames :: [Statement] -> [Statement]
mangleNames = id
