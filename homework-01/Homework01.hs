{-|
CIS 194 Homework 1

Problems: https://www.seas.upenn.edu/~cis1940/spring13/hw/01-intro.pdf
-}

module Homework01 where

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x > 0     = mod x 10 : toDigitsRev (div x 10)
  | otherwise = []

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherRev . reverse
  where doubleEveryOtherRev (x0:x1:xs) = x0 : 2 * x1 : doubleEveryOtherRev xs
        doubleEveryOtherRev xs = xs

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits
