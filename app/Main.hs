module Main where

import Lib

main :: IO ()
main = do
    putStrLn "Please enter a Message: "
    message <- getLine
    putStrLn "unencrypted message: "
    multiKey (read message)
