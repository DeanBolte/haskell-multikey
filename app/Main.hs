module Main where

import Lib

main :: IO ()
main = do
    putStrLn "Please enter a Message: "
    message <- getLine
    multiKey (read message)
