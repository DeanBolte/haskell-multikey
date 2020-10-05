module Lib
    ( multiKey
    ) where

import Prelude

multiKey :: Int -> IO ()
multiKey message = invmod 3 26

invmod :: Int -> Int -> Maybe Int
invmod a m = let invmod' (g, x, _) | g /= 1 = Nothing
                                   | otherwise = Just (x `mod` m)
             in invmod' $ egcd a m

egcd :: Int -> Int -> (Int, Int, Int)
egcd 0 b = (b, 0, 1)
egcd a b = let out (g, y, x) = (g, x - (b `div` a) * y, y)
           in out $ egcd (b `mod` a) a