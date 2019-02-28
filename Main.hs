module Main where

import System.IO
import Parser
import ParseProg
import Language
import Printer

main :: IO CoreProgram
main = test1 "input.txt"

test1 :: String -> IO CoreProgram
test1 fn = do inp <- readFile fn
              return (comp (parse parseProg inp))

test2 :: String -> IO ()
test2 fn = do inp <- readFile fn
              putStrLn (pprint (comp (parse parseProg inp)))

comp :: [(CoreProgram, Name)] -> CoreProgram
comp [] = error "No parse"
comp [(e,[])] = e
comp [(_,a)] = error ("Doesn't use all input:" ++ a)