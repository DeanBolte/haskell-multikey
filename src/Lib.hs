module Lib
    ( multiKey
    ) where

import Prelude
import System.Random 
import Data.Bits
import Math.NumberTheory.Primes.Testing
import Control.Monad.Fix

multiKey :: Integer -> IO ()
multiKey message = do
    -- Bank
    (p, q) <- rndPrimes 64
    let n = p * q
        phi = (p - 1) * (q - 1)
    k1 <- rndPrime 32
    k2 <- rndPrime 32
    k3 <- rndPrime 32
    let k4 = parseMaybeInt $ (k1 * k2 * k3) `invmod` phi
    -- Alice, Bob, Karen
    let s = powerMod n message (k1 * k3 * k4)
    putStrLn "encrypted message: "
    print s
    -- Verification
    let m' = powerMod n s k2
    putStrLn "unencrypted message: "
    print m'

powerMod :: Integral a => a -> a -> a -> a
powerMod _ _ 0 = 1
powerMod n b 1 = b `mod` n
powerMod n b e
    | even e = powerMod n squareMod (e `div` 2)
    | otherwise = (b * powerMod n squareMod ((e - 1) `div` 2)) `mod` n
    where squareMod = b * (b `mod` n)

rndPrime :: Int -> IO Integer
rndPrime bits =
    fix $ \again -> do
        x <- fmap (.|. 1) $ randomRIO (2^(bits - 1), 2^bits - 1)
        if isPrime x then return x else again

rndPrimes :: Int -> IO (Integer, Integer)
rndPrimes bits = do
    p <- rndPrime bits
    fix $ \again -> do
        q <- rndPrime bits
        if p /= q then return (p, q) else again

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