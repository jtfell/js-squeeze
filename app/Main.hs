module Main where

import System.Environment (getArgs)
import System.IO (Handle, hClose, hGetContents, openFile, IOMode(ReadMode))

import Text.Parsec

import Lexer
import Parser
import AST

main :: IO ()
main = do
    filePath:xs <- getArgs
    hdl <- openFile filePath ReadMode
    str <- hGetContents hdl
    print str
    let parsed = runParser (contents program) () filePath str
    print parsed

    hClose hdl
