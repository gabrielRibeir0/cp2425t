import Cp
import List

--ana
g :: [Int] -> Either () ((Int, [Int]), [Int])
g = (id -|-  (split (split head id) tail . cons)) . outList . iSort 

--funções auxiliares 
pp1 = uncurry (/=) . split (p1.p2) (const 0) 
pp2 = uncurry (<=) . split (p1.p2) (length.p2.p2)

--cata
f :: Either () ((Int, [Int]), (Int, [Int])) -> (Int, [Int])
f = either (split (const 0) nil) (cond (uncurry (&&) . split pp1 pp2 ) p2 p1)

hindex :: [Int] -> (Int,[Int])
hindex = hyloList f g