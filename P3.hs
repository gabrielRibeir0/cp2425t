import Cp
import List
import Data.List

--map pode ser um catamorfismo
createRow :: Num a => [a] -> a -> [a]
createRow l1 y = map (*y) l1


--createMatrix [] _ = []
--createMatrix _ [] = []
--createMatrix l1 (y:ys) = createRow l1 y : createMatrix l1 ys

createMatrix :: Num a => [a] -> [a] -> [[a]]
createMatrix l1 = anaList ((id -|- createRow l1 >< id) . outList)

--Tentar juntar esta com a de cima no mesmo anamorfismo // ou transformar esta num cata/anamorfismo // usar os combinadores de CP ?
diagonals :: Num a => [[a]] -> [[a]]
diagonals [] = []
diagonals [row] = map singl row
diagonals (row : rows) = extendDiagonals row (diagonals rows)
  where
    extendDiagonals row diags = zipWith (:) row ([] : diags) ++ drop (length row - 1) diags

--Também pode ser só assim, mas não usa combinadores, não sei qual a mais apreciada
--sumDiagonals = map sum
sumDiagonals :: Num a => [[a]] -> [a]
sumDiagonals = cataList (either nil (cons . (sum >< id)))

--sumDiagonals = map sum
convolve :: Num a => [a] -> [a] -> [a]
convolve l1 = sumDiagonals . diagonals . createMatrix l1


convolve2 :: Num a => [a] -> [a] -> [a]
convolve2 hs = map (sum . zipWith (*) (reverse hs)) . init . tails . (++) pad
  where pad = replicate (length hs - 1) 0