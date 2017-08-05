module Minifier where

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

--
-- Eliminate dead code
--
-- eg.
--  function (a) {
--    var b = a + 2;
--    return b;
--    console.log(x);
--  }
--       ||
--       \/
--
--  function (a) {
--    var b = a + 2;
--    return b;
--  }
--
eliminateDeadCode :: [Statement] -> [Statement]
eliminateDeadCode [] = []
eliminateDeadCode (FunctionDeclaration (Function id lam):xs) = FunctionDeclaration (Function id (lookInsideLambda lam)):eliminateDeadCode xs
eliminateDeadCode (a:xs) = a:eliminateDeadCode xs

lookInsideLambda :: Lambda -> Lambda
lookInsideLambda (Lambda p (Block ss)) = Lambda p (Block (removeAfterReturn ss))

removeAfterReturn :: [Statement] -> [Statement]
removeAfterReturn [] = []
removeAfterReturn (ReturnStatement s:xs) = [ReturnStatement s]
removeAfterReturn (s:xs) = s:removeAfterReturn xs

--
-- Mangle names
--
-- eg. var longName = 'good';console.log(longName); => var a = 'good';console.log(a);
--
mangleNames :: [Statement] -> [Statement]
mangleNames = id

minify :: Program -> Program
minify = Program . joinConsecutiveVars . eliminateDeadCode . mangleNames . fromProgram

fromProgram :: Program -> [Statement]
fromProgram (Program statements) = statements
