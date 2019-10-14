-- 3. Generating Lists

import Test.QuickCheck
import Text.Show.Functions

import Data.Char
import Data.Maybe
import Data.List

listOfLength :: Int -> Gen a -> Gen [a]
listOfLength n gen = sequence [ gen | i <- [1..n] ]