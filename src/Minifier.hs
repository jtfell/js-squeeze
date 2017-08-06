module Minifier where

import AST

import Minifier.JoinConsecutiveVars (joinConsecutiveVars)
import Minifier.EliminateDeadCode (eliminateDeadCode)
import Minifier.MangleNames (mangleNames)

minify :: Program -> Program
minify = Program . joinConsecutiveVars . eliminateDeadCode . mangleNames . fromProgram

fromProgram :: Program -> [Statement]
fromProgram (Program statements) = statements
