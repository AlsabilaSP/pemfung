import Control.Monad.State.Lazy
import Control.Monad.State

{-
'return' set the result value but leave the state unchanged.
'get' set the result value to the state and leave the state unchanged.
'put' set the result value to () and set the state value.
Because (State s) forms a monad, values can be combined with (>>=) or do{}.

Misal contoh kode berikut:
postincrement = do { x <- get; put (x+1); return x }
runState postincrement 1
Outputnya adalah (1,2)

predecrement = do { x <- get; put (x-1); get }
runState predecrement 1
Outputnya adalah (0,0)
-}

-- Example use of State monad
-- Passes a string of dictionary {a,b,c}
-- Game is to produce a number from the string.
-- By default the game is off, a C toggles the
-- game on and off. A 'a' gives +1 and a b gives -1.
-- E.g 
-- 'ab'    = 0
-- 'ca'    = 1
-- 'cabca' = 0
-- State = game is on or off & current score
--       = (Bool, Int)

type GameValue = Int
type GameState = (Bool, Int)

playGame :: String -> State GameState GameValue
playGame []     = do
    (_, score) <- get
    return score

playGame (x:xs) = do
    (on, score) <- get
    case x of
         'a' | on -> put (on, score + 1)
         'b' | on -> put (on, score - 1)
         'c'      -> put (not on, score)
         _        -> put (on, score)
    playGame xs

startState = (False, 0)

main = print $ execState (playGame "abcaaacbbcabbab") startState
-- EvalState and execState just select one of the two values returned by runState.
-- EvalState returns the final result 2 
-- while execState returns the final state (True, 2)