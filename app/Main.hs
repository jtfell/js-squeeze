module Main where

import System.Environment (getArgs)
import System.IO (Handle, hClose, hGetContents, openFile, IOMode(ReadMode))

import Lexer
import Parser
import AST

main :: IO ()
main = do
    filePath:xs <- getArgs
    hdl <- openFile filePath ReadMode
    str <- hGetContents hdl
    print str
    hClose hdl
