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

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n - 1) a c b ++ [(a,b)] ++ hanoi (n - 1) c b a

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 a b _ _ = [(a,b)]
hanoi4 n a b c d = hanoi4 (n - 2) a c b d
                   ++ [(a,d),(a,b),(d,b)]
                   ++ hanoi4 (n - 2) c b a d
