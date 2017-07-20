module Main where

import Lib

main = do
    printBoard example
    putStrLn "==="
    mapM p $ solutions example
    where p board = do printBoard board
                       putStrLn "==="
