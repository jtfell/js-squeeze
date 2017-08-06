module Minifier.JoinConsecutiveVars (joinConsecutiveVars) where

import AST
import Data.Text as T

--
-- Combine var declarations
--
-- eg. var x;var y;var z; => var x,y,z;
--
joinConsecutiveVars :: [Statement] -> [Statement]
-- Base case
joinConsecutiveVars [] = []
-- Basic case of chained var decls
joinConsecutiveVars (VariableDeclaration varDec1:VariableDeclaration varDec2:xs) =
    joinConsecutiveVars (VariableDeclaration (combineDecls varDec1 varDec2):xs)
-- Check inside function blocks
joinConsecutiveVars (FunctionDeclaration (Function id (Lambda p (Block ss))):xs) = FunctionDeclaration (Function id (Lambda p (Block $ joinConsecutiveVars ss))):joinConsecutiveVars xs
-- Recursive case
joinConsecutiveVars (a:xs) = a:joinConsecutiveVars xs

combineDecls :: VariableDecl -> VariableDecl -> VariableDecl
combineDecls (VariableDecl decl1) (VariableDecl decl2) = VariableDecl $ mappend decl1 decl2
