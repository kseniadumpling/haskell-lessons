{- 
    Теория по λ-исчислению: 

    E ::= x | y | z ...
        | λx.E
        | E E
        | (E)


    α-эквивалентность: 
    λx.E = λy.E[y]

    β-редукция: 
    (λx.E1)E2 -> E1[E2]

    η-преобразование: 

    δ-редукция:
    (вводит некоторые константы)

-}

{-
    true = λx.λy.x
    false = λx.λy.y

    0 = λf.λx.x
    1 = λf.λx.fx
    2 = λf.λx.f(fx)

    flip = λf.λx.λy.fyx (по η-преобразованию: flip f x y = f y x)

    Структура данных: упорядоченные пары
    pair x y = λf.fxy  (//pair = λx.λy.λf.fxy)

    fst p = p true
    snd p = p false
    //swap p = pair (snd p) (fst p)

    Пример: 
    fst(pair 5 6) ≡ fst[(λx.λy.λf.fxy) 5 6] =
        = fst[(λy.λf.f 5 y) 6] = fst[λf.f 5 6] =
        = (λp.p true)[λf.f 5 6] = [λf.f 5 6] true = 
        = true 5 6 ≡ (λx.λy.x) 5 6 = (λy.5)6 = 5
    
-}
{-
    Примечания: 

    Тип данных Unit (аналог типа void в Си)
    ()::Unit

    В языках программирования (примечание про память и контекст): 
    let x = E1 in E2 - добавление пары имя-значение в контекст следующих выражений
    let p = pair 5 6 in fst p
    letrec (LISP, в Haskell по умолчанию) - используем один общий контекст на всех

    Структура данных список - это пара число-пара

-}

{-
    Введем функции для пары: 
    nil = λf.true
    empty? p = p(λx.λy.false)

    Примеры: 
    empty? (pair 1 nil) = empty? (λf.f 1 nil) = (λf.f 1 nil)(λx.λy.false) = false
    empty? nil = nil(λx.λy.false) ≡ (λf.true)(λx.λy.false) = true

    Рассмотрим декремент:
    Введем функцию, которая имеет "память":
    f(x,y) = (y, y+1)
    пример:
    f(0,0) -> (0,1)
    f(0,1) -> (1,2)
    f(1,2) -> (2,3)

    Осталось только возвращать первый аргумент
-}

{-
    
-}


module Example where

    len :: [Integer] -> Integer
    len [] = 0
    len (1 : 3 : 15 : []) = 64
    len (x : xs) = 1 + len xs

    data MyList a = Nil | Cons a (MyList a)
        deriving Show

    myLength Nil = 0
    myLength (Cons x xs) = x + myLength(xs)

    -- Return incremented argument
    f :: Int -> Int
    f x = x + 1

    {-
        Return multiplied arguments
    -}
    g :: (Num a) => a -> a -> a
    g x y = x * y

    -- factorial :: Integer -> Integer
    factorial n
        | n <= 1 = 1
        | otherwise = n * factorial (n - 1)

    fact n = product[1..n]

    factorialInt :: Int -> Int
    factorialInt n
        | n <= 1 = 1
        | otherwise = n * factorialInt (n - 1)

    main :: IO ()
    main = do 
        putStr "Hello, "
        putStrLn "World"