module Minifier.EliminateDeadCode (eliminateDeadCode) where

import AST
import Data.Text as T
import Data.Map as M

import Control.Lens

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
eliminateDeadCode (FunctionDeclaration fn:xs) =
    FunctionDeclaration (applyToLambda fn):eliminateDeadCode xs
eliminateDeadCode (a:xs) = a:eliminateDeadCode xs

applyToLambda :: Function -> Function
applyToLambda = over (lam . blk . statements) removeAfterReturn

removeAfterReturn :: [Statement] -> [Statement]
removeAfterReturn [] = []
removeAfterReturn (ReturnStatement s:xs) = [ReturnStatement s]
removeAfterReturn (s:xs) = s:removeAfterReturn xs


