{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (P p1) == (P p2) = p1 == p2 
 
-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P arr) = displayArr 0 arr
        where
        displayArr _ [] = ""
        displayArr len [x] = displayTerm len x
        displayArr len (x:xs)
            | x == 0 = displayArr (len + 1) xs
            | otherwise = (displayArr (len + 1) xs) ++ " + " ++ (displayTerm len x) 

        displayTerm _ 0 = ""
        displayTerm 0 coef = show coef
        displayTerm 1 1 = "x"
        displayTerm 1 (-1) = "-x"
        displayTerm 1 coef = (show coef) ++ "x"
        displayTerm pow 1 = "x^" ++ (show pow)
        displayTerm pow coef = (show coef) ++ "x^" ++ (show pow)


-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P coefs1) (P coefs2) = P (addCoefs coefs1 coefs2)
    where
    addCoefs [] [] = []
    addCoefs [x] [] = [x]
    addCoefs [] [x] = [x]
    addCoefs (x:xs) [] = x : (addCoefs xs [])
    addCoefs [] (y:ys) = y : (addCoefs [] ys)
    addCoefs (x:xs) (y:ys) = (x + y) : (addCoefs xs ys)

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P coefs1) (P coefs2) = foldl (+) (P [0]) $ multiplyCoefs coefs1 coefs2 
    where
    multiplyCoefs [] _ = []
    multiplyCoefs (x:xs) coefs = (P (map (* x) coefs)) : (multiplyCoefs xs (0 : coefs))

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = (* P [(-1)])
    fromInteger x = P ([fromInteger x])
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P coefs) val = evaluateCoefs 0 coefs val
    where
    evaluateCoefs _ [] val = 0
    evaluateCoefs acc [x] val = x * (val ^ acc)
    evaluateCoefs acc (x:xs) val = x * (val ^ acc) + (evaluateCoefs (acc + 1) xs val)

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 0 f = f
    nderiv n f = deriv (nderiv (n - 1) f) 

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P coefs) = P (derivCoefs 0 coefs)
        where
        derivCoefs _ [] = []
        derivCoefs pow (x:xs) = (x * pow) : (derivCoefs (pow + 1) xs)