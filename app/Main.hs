module Main where

import Lib
import Globals
import System.Environment

consoleInput = getLine

fileInput = do
    args <- getArgs
    putStrLn $ "Parsing file " ++ (args !! 0)
    content <- readFile (args !! 0)
    if debugEchoFileContents
        then putStrLn content
        else pure ()
    return content
             
main = do 
    text <- fileInput
    let  parseResult = parseAny text []
    case parseResult of
        Left e -> putStrLn $ "ParseError " ++ show e
        Right p -> do 
            if debugPrintProgram
                then putStrLn $ show p
                else pure ()
            newTape <- execute p (Tape [] 0 [])
            if debugPrintTape
                then putStrLn $ show newTape
                else pure ()
