{-# OPTIONS_GHC -fwarn-tabs #-}

module HaskellIntro where

import Set

-- Load this file into GHCi (say, with `ghci HaskellIntro.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

isThisWorking :: String
isThisWorking = "Yes"

--
-- Problem 1
--

lastDigit :: Integer -> Integer
lastDigit a = mod a 10

dropLastDigit :: Integer -> Integer
dropLastDigit a = a `div` 10

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

toDigits :: Integer -> [Integer]
toDigits n | n == 0 = []
           | n < 0 = []
           | otherwise = (toDigits (dropLastDigit n)) ++ [lastDigit n]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther a | a == [] = []
                   | length a == 1 = [head a]
                   | otherwise = (doubleEveryOther (init (init a))) ++ [2*(last (init a)), last a]

sumDigits :: [Integer] -> Integer
sumDigits a | a == [] = 0
            | a == [0] = 0
            | length a == 1 =  (head (toDigits (head a))) + (sumDigits (tail (toDigits (head a))))
            | otherwise = (sumDigits (toDigits (head a))) + (sumDigits (tail a))

validate :: Integer -> Bool
validate a = (mod (sumDigits (doubleEveryOther (toDigits a))) 10) == 0

--
-- Problem 2
--

pow :: (a -> a) -> Int -> a -> a
pow func n x | n == 0 = x
             | otherwise = (pow func (n-1) (func x))

g :: Integer -> Integer
g x | x == 0 = 0
    | otherwise = x - pow g 2 (x-1) 

h :: Integer -> Integer
h x | x == 0 = 0
    | otherwise = x - pow h 3 (x-1) 

d :: Int -> Integer -> Integer
d n x | x == 0 = 0
      | otherwise = x - (pow (d n) n (x-1))

--
-- Problem 3
--

powerSet :: (Ord a) => Set a -> Set (Set a)
powerSet xs | isEmpty xs = singleton empty
            | otherwise = union (mapSet (insert (fst (split xs))) (powerSet (snd (split xs)))) (powerSet (snd (split xs)))
