{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task1 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (filter, foldl, foldr, head, length, map, read, reverse, show, sum, tail)

-----------------------------------
--
-- Checks whether the last digit is a valid check digit
-- for the rest of the given number using Luhn algorithm
--
-- Usage example:
--
-- >>> validate 3456
-- False
-- >>> validate 34561
-- True
-- >>> validate 34562
-- False

validate :: Integer -> Bool
validate n = luhn initDigits == checkDigit
  where
    digits = toDigits n
    initDigits = init digits
    checkDigit = last digits

-----------------------------------
--
-- Computes check digit for given digits using Luhn algorithm
--
-- Usage example:
--
-- >>> luhn [3,4,5,6]
-- 1

luhn :: [Int] -> Int
luhn digits = mod (10 - mod s 10) 10
  where
  s = sum (map normalize (doubleEveryOther (reverse digits)))

-----------------------------------
--
-- Produces list of digits for given positive number;
-- otherwise (for zero and negative numbers) returns empty list
--
-- Usage example:
--
-- >>> toDigits 3456
-- [3,4,5,6]
-- >>> toDigits 0
-- []
-- >>> toDigits (-123)
-- []

toDigits :: Integer -> [Int]
toDigits 0 = []
toDigits n = toDigits (div n 10) ++ [fromInteger (mod n 10)]

-----------------------------------
--
-- Produces list in reverse order to the given one
--
-- Usage example:
--
-- >>> reverse "Hello"
-- "olleH"
-- >>> reverse [3,4,5,6]
-- [6,5,4,3]

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

-----------------------------------
--
-- Doubles every other digit starting from first one
--
-- Usage example:
--
-- >>> doubleEveryOther [6,5,4,3]
-- [12,5,8,3]

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther []          = []
doubleEveryOther [x]         = [x + x]
doubleEveryOther (x:y:other) = (x + x) : y : doubleEveryOther other

-----------------------------------
--
-- Normalizes given number to single digit by subtracting 9
-- if it is greater than or equal to 10
--
-- (Assumes inputs between 0 and 18)
--
-- Usage example:
--
-- >>> normalize 12
-- 3
-- >>> normalize 1
-- 1

normalize :: Int -> Int
normalize x 
  | x >= 9    = x - 9 
  | otherwise = x

-----------------------------------
--
-- Produces list with given function applied to each element
-- in given list
--
-- Usage example:
--
-- >>> map (\x -> x * 2) [1,2,3,4]
-- [2,4,6,8]

map :: (a -> b) -> [a] -> [b]
map _ []         = []
map f (x:others) = f x : map f others

-----------------------------------
--
-- Computes sum of given list of numbers
--
-- Usage example:
--
-- >>> sum [3,8,5,3]
-- 19
-- >>> sum []
-- 0

sum :: [Int] -> Int
sum []         = 0 
sum (x:others) = x + sum others

