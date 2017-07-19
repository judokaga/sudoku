module Main where

import Lib

main = do
    printBoard' example
    putStrLn "==="
    printBoard' $ solutions example
