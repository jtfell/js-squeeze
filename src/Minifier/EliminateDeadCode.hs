module Minifier.EliminateDeadCode (eliminateDeadCode) where

import AST
import Data.Text as T
import Data.Map as M

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
eliminateDeadCode (FunctionDeclaration (Function id lam):xs) =
    FunctionDeclaration (Function id (lookInsideLambda lam)):eliminateDeadCode xs
eliminateDeadCode (a:xs) = a:eliminateDeadCode xs

lookInsideLambda :: Lambda -> Lambda
lookInsideLambda (Lambda p (Block ss)) = Lambda p (Block (removeAfterReturn ss))

removeAfterReturn :: [Statement] -> [Statement]
removeAfterReturn [] = []
removeAfterReturn (ReturnStatement s:xs) = [ReturnStatement s]
removeAfterReturn (s:xs) = s:removeAfterReturn xs
