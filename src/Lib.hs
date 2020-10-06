module Lib
    ( multiKey
    ) where

import Prelude

multiKey :: Integer -> Integer -> Integer -> Integer -> IO ()
multiKey message k1 k2 k3 = do
    -- Bank
    let p = 929
    let q = 863
    let n = p * q
    let phi = (p - 1) * (q - 1)
    let k4 = parseMaybeInt $ (k1 * k2 * k3) `invmod` phi
    print k4
    -- Alice, Bob, Karen
    let s = powerMod n message (k1 * k3 * k4)
    print s
    -- Verification
    let m' = powerMod n s k2
    print m'

powerMod :: Integral a => a -> a -> a -> a
powerMod _ _ 0 = 1
powerMod n b 1 = b `mod` n
powerMod n b e
    | even e = powerMod n squareMod (e `div` 2)
    | otherwise = (b * powerMod n squareMod ((e - 1) `div` 2)) `mod` n
    where squareMod = b * (b `mod` n)

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