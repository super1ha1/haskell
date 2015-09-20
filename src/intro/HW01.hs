{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = mod n 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits  n 
	| n <= 0 = []
	| otherwise =  (lastDigit n) : toRevDigits(dropLastDigit n) 

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] =  []
--doubleEveryOther (a : b ) = a : (b + b) : []
doubleEveryOther (a: b :c ) = a : (b + b) : doubleEveryOther c


-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits  (a : b :c ) = (sumDigit a) + b +  (sumDigits c)


sumDigit :: Integer -> Integer
sumDigit n 
		 |  n <= 0  = 0 
		 |  n < 10  = n 
		 | otherwise = (lastDigit n) + (sumDigit (dropLastDigit n))
-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n 
	  | n <= 0 = False
	  | (mod (sumDigits (doubleEveryOther (toRevDigits n))) 10) == 0 =  True
	  | otherwise = False


-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 src des tmp = []
hanoi 1 src des tmp = [(src,des)]
hanoi n src des tmp = [] ++ (hanoi (n-1) src tmp des) ++ (hanoi 1 src des tmp) ++ (hanoi (n-1) tmp des src) 

hanoi4 :: Integer -> Integer
hanoi4 0  = 0
hanoi4 1  = 1
--hanoi4 n = minimum $ zip  [] [0..n-1] []
--hanoi4 n src des tmp1 tmp2 = ( (hanoi 1 src des tmp1 tmp2 +  (2^ (n -1)  -1 )), () )


hanoiCount ::  Integer -> Integer -> Integer
hanoiCount n k = (hanoi4 k ) + (2 ^ (n-k) -1)