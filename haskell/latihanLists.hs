import Data.List

-- 1. Permutations
-- removeOnce x xs removes x from the list xs, but only once
removeOnce :: Eq a => a -> [a] -> [a]
removeOnce x []                 = []
removeOnce x (y:ys) | x == y    = ys
                    | otherwise = y : removeOnce x ys
-- isPermutation xs ys checks whether xs is a permutation of ys
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation []     []     = True
isPermutation []     (y:ys) = False
isPermutation (x:xs) ys     = exists x ys && isPermutation xs (removeOnce x ys)
-- Saya mengerti cara kerjanya tapi tidak bisa diterapkan sendiri. Sepertinya karena masalah error modul Test.QuickCheck tidak ada :')

