import Cp
import List

createRow :: [Int] -> Int -> [Int]
createRow l y = map (*y) l


--createMatrix [] _ = []
--createMatrix _ [] = []
--createMatrix l1 (y:ys) = createRow l1 y : createMatrix l1 ys

createMatrix :: [Int] -> [Int] -> [[Int]]
createMatrix l1 = anaList ((id -|- createRow l1 >< id) . outList)

printMatrix :: [[Int]] -> String
printMatrix = unlines . map (unwords . map show)