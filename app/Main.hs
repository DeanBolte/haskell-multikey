module Main where

import Lib

main :: IO ()
main = do
    putStrLn "Please enter a Message: "
    message <- getLine
    putStrLn "Enter k1: "
    k1 <- getLine
    putStrLn "Enter k2: "
    k2 <- getLine
    putStrLn "Enter k3: "
    k3 <- getLine
    putStrLn "unencrypted message: "
    multiKey (read message) (read k1) (read k2) (read k3)
