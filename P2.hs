import Cp
import List
import Exp

outPrimes 0 = i1 ()
outPrimes 1 = i1 ()
outPrimes (-1) = i1 ()
outPrimes n = i2 (abs n)

divisorsList :: Int -> [Int]
divisorsList n = [x | x <- [2..isqrt n], mod n x == 0]
    where isqrt = floor . sqrt . fromIntegral

isPrime :: Int -> Bool
isPrime = either false (null . divisorsList) . outPrimes

nextPrimeFactor :: Int -> Int
nextPrimeFactor n = head [x | x <- 2:[3,5..n], mod n x == 0 && isPrime x]

primes :: Int -> [Int]
primes = anaList ((id -|- split nextPrimeFactor (aap div nextPrimeFactor)) . outPrimes)


buildPairs :: [Int] -> [([Int], Int)]
buildPairs = map (split ((:) 1 . primes) id)

prime_tree :: [Int] -> Exp Int Int
prime_tree = head . untar . buildPairs 