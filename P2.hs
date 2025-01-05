import Cp
import List

outPrimes 0 = i1 ()
outPrimes 1 = i1 ()
outPrimes (-1) = i1 ()
outPrimes n = i2 (abs n)

isPrimeAux :: Int -> Bool
isPrimeAux n = (either true false . outList) [x | x <- [2..isqrt n], mod n x == 0]
    where isqrt = floor . sqrt . fromIntegral

isPrime :: Int -> Bool
isPrime = either false isPrimeAux . outPrimes

nextPrimeFactor :: Int -> Int
nextPrimeFactor n = head [x | x <- 2:[3,5..n], mod n x == 0 && isPrime x]

--Explorar mais isto para as funções de criar listas acima (não sei se é mesmo preciso)
a :: Integer -> Integer  -> Bool
a x = (== 0) . flip mod x

--Ver melhor como aap funciona, se não se perceber, deixar estar o divide
--divide :: Int -> Int
--divide n = div n (nextPrimeFactor n)

primes :: Int -> [Int]
primes = anaList ((id -|- split nextPrimeFactor (aap div nextPrimeFactor)) . outPrimes)