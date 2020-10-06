module Lib
    ( multiKey
    ) where

import Prelude

multiKey :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> IO ()
multiKey message p q k1 k2 k3 = do
    -- Bank
    let n = p * q
    let phi = (p - 1) * (q - 1)
    let k4 = parseMaybeInt $ (k1 * k2 * k3) `invmod` phi
    print k4
    -- Alice, Bob, Karen
    let s = (message ^ (k1 * k3 * k4)) `mod` n
    print s
    -- Verification
    let m' = (s ^ k2) `mod` n
    print m'

parseMaybeInt :: Maybe Integer -> Integer
parseMaybeInt (Just x) = x
parseMaybeInt Nothing = 0

invmod :: Integer -> Integer -> Maybe Integer
invmod a m = let invmod' (g, x, _) | g /= 1 = Nothing
                                   | otherwise = Just (x `mod` m)
             in invmod' $ egcd a m

egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd 0 b = (b, 0, 1)
egcd a b = let out (g, y, x) = (g, x - (b `div` a) * y, y)
           in out $ egcd (b `mod` a) a