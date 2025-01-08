{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Cp
import List
import Exp
import Data.List (nub)
import LTree (cataLTree)

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

groupPrimes :: [Int] -> [(Int, [Int])]
groupPrimes nums = (cataList (either nil (cons . (split id (divisibleNumbers nums) >< id))) . nub . concatMap primes) nums
    where divisibleNumbers nums p = [n | n <- nums, mod n p == 0]

subTree :: (Int, [Int]) -> Exp Int Int
subTree (p, ns)
      | all (all (==p) . primes) ns = Var (product ns)  -- If p is the only prime factor left
      | otherwise = Term p ([subTree (q, [n `div` p | n <- ns, mod n q == 0]) |
                               q <- nub $ concatMap (primes . flip div p) ns])

prime_tree :: [Int] -> Exp Int Int
--prime_tree nums = Term 1 (map subTree (groupPrimes nums)) 
prime_tree = hyloList cata ana

--[n, n1, ...] -> [(n, primes n), (n1, primes n1), ...]
ana :: [Int] -> Either () ([[Int]], [Int])
ana = (id -|- split (map primes . cons) id) . outList

cata :: Either () ([[Int]], Exp Int Int) -> Exp Int Int
cata 

