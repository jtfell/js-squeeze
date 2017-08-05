module Main where

import System.Environment (getArgs)
import System.IO (Handle, hClose, hGetContents, openFile, IOMode(ReadMode))

import Text.Parsec
import Data.Text

import Lexer
import Parser
import AST
import CodeGen
import Minifier

main :: IO ()
main = do
    filePath:xs <- getArgs

    -- Get the source contents
    hdl <- openFile filePath ReadMode
    str <- hGetContents hdl

    -- Run the parser to convert to an AST
    let parsed = runParser (contents program) () "" str

    -- If valid syntax, we minify the program and run the code generation step
    -- Otherwise, print the error
    case parsed of
        Right prog -> print $ gen $ minify prog
        Left err -> print err

    -- Close the file
    hClose hdl
