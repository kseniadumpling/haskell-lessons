import Data.Maybe

{- Task 1 -}

-- Computes all divisors of the integer number (including the number itself)
-- (actually the case of negative number is debatable, the task isn't really clear for this part)
divisorsAbs :: Integer -> [Integer]
divisorsAbs n = [ x | x <- [1..(div n 2)], mod n x == 0] ++ [n]

divisors n
    | n >= 0 = divisorsAbs n
    | otherwise = divisorsAbs (abs n) ++ map (* (-1)) (divisorsAbs (abs n))

-- Checks if the number is perfect i.e. equivalent to sum of its divisors
-- For this case I'll check only positive numbers 
isPerfect :: Integer -> Bool
isPerfect n
    | n <= 1 = False
    | otherwise = sum (init (divisorsAbs n)) == n



{- Task 2 -}    

-- Splits a list into two smaller lists (at the Nth position).
splitList :: Int -> [a] -> ([a], [a])
splitList n xs = splitAt n xs
{- or just splitList n xs = (take n xs, drop n xs) -}

-- Splits a list into two smaller lists, for the first one it inspects the original 
-- list and takes from it its elements to the moment when the condition fails, 
-- then it takes the other part for the second one
splitBy :: (a -> Bool) -> [a] -> ([a], [a])
splitBy cond xs = (takeWhile cond xs, dropWhile cond xs)

-- Сreates an array of string from the original one, space characters serving as separators
-- (there is nothing new under the sun... especially bicycles)
splitWords :: String -> [String]
splitWords str = case dropWhile (\ sym -> sym == ' ') str of
    "" -> []
    str' -> w : splitWords str''
        where (w, str'') = break (\ sym -> sym == ' ') str' 



{- Task 3 -}

-- Defines an Expr data type for representing arithmetic expressions       
data Expr = Const Double
    | Var String
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr

-- Prints an arithmetic expression in infix form
instance Show Expr where
    show (Const c) = show c
    show (Var v) = show v
    show (Add a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Sub a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
    show (Mul a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
    show (Div a b) = "(" ++ show a ++ " / " ++ show b ++ ")"

-- Defines an Env data type for representing a context in which the calculation is performed
-- as a pair (key, value)
type Env = [(String, Double)]

-- Calculates arithmetic expression
eval :: Env -> Expr -> Double
eval env (Const c) = c
eval env (Var v) = case lookup v env of
    Nothing -> 0
    Just a -> a
eval env (Add a b) = (eval env a) + (eval env b)
eval env (Sub a b) = (eval env a) - (eval env b)
eval env (Mul a b) = (eval env a) * (eval env b)
eval env (Div a b) = (eval env a) / (eval env b)

