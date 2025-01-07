import Cp
import List

g :: [Int] -> Either () ((Int, [Int]), [Int])
--g [] = i1 ()
--g ys = i2 ((head xs, xs), tail xs)
  --  where xs = iSort ys
g = (id -|- split (split head id) tail . cons) . outList . iSort

f :: Either () ((Int, [Int]), (Int, [Int])) -> (Int, [Int])
--f (Left ()) = (0,[])
--f (Right ((i,xs), prev)) = if i >= length xs 
  --                         then (i,xs)
    --                       else prev

f = either (const (0,[])) (cond (uncurry (>=) . split (p1.p1) (length.p2.p1)) p1 p2)


hIndex :: [Int] -> (Int,[Int])
hIndex = hyloList f g

