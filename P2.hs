import Cp
import List

out 0 = i1 ()
out 1 = i1 ()
out (-1) = i1 ()
out n = i2 (abs n)

isPrime :: Int -> Bool
isPrime n | n < 2     = False
          | otherwise = null [x | x <- [2..isqrt n], mod n x == 0]
    where isqrt = floor . sqrt . fromIntegral

nextPrimeFactor :: Int -> Int
nextPrimeFactor n = head [x | x <- 2:[3,5..n], mod n x == 0 && isPrime x]

divide :: Int -> Int
divide n = div n (nextPrimeFactor n)

primes :: Int -> [Int]
primes = anaList ((id -|- split nextPrimeFactor divide) . out)