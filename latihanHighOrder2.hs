-- 2 (*). List Comprehensions and Higher-Order Functions

import Text.Show.Functions

import Data.Char
import Data.Maybe
import Data.List

-- HIGHER-ORDER FUNCTION
no1a xs = map (+1) xs
no2a xs ys = concat (map (\x -> map (\y -> x+y) ys) xs) 
no3a xs = map (+2) $ filter (>3) xs
-- Saya tidak mengerti maksud no 4 dan 5 :')
no6a mxs = map (\(Just x) -> x+5) (filter isJust mxs)
-- Jadi misalnya ada mxs = [Just 3, Just 5, Nothing], outpunya adalah [8,10]  

-- LIST COMPREHENSIONS
no1b xs = [ x+3 | x <- xs :: [Int] ]
no2b xs = [ x | x <- xs :: [Int], x > 7 ]
no3b xs ys = [(x,y) | x <- xs, y <- ys ]
no4b xys = [ x+y | (x,y) <- xys, x+y > 3 ]