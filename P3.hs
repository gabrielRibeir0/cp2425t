import Cp
import List
import Nat

convolve :: Num a => [a] -> [a] -> [a]
convolve hs = hyloList f g . suffixes . flip padZeros (length hs - 1)
  where padZeros l = cataNat (either (const l) (0:))
        f :: Num a => Either () ([a], [a]) -> [a]
        f = either nil (cons . (sum >< id))
        g = (id -|- (zipWith (*) (reverse' hs) >< id)) . outList