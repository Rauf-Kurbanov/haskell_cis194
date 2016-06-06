-- http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf
module Main where

import Data.Char (digitToInt)
import Test.HUnit

-- exercise 1
toDigits :: Integer -> [Integer]
toDigits num
  | num <= 0  = []
  | otherwise = map (toInteger . digitToInt) $ show num

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

testEx1 :: Test
testEx1 = test [ toDigits 1234 ~=? [1,2,3,4]
               , toDigitsRev 1234 ~=? [4,3,2,1]
               , toDigits 0 ~=? []
               , toDigits (-17) ~=? []
               ]

-- exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEven . reverse
  where
    doubleEven (a:b:xs) = a : 2*b : doubleEven xs
    doubleEven xs       = xs

testEx2 :: Test
testEx2 = test [ doubleEveryOther [8,7,6,5] ~=? [16,7,12,5]
               , doubleEveryOther [1,2,3] ~=? [1,4,3]
               ]

-- exercise 3
sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

-- exercise 4
validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits

testEx4 :: Test
testEx4 = test [ validate 4012888888881881 ~=? True
               , validate 4012888888881882 ~=? False
               ]

-- exercise 5
type Peg = String
type Move = (Peg, Peg)

-- move n discs (stacked in increasing size) from peg a to peg b
-- using peg c as temporary storage
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _    = []
hanoi n pa pb pc = hanoi (n-1) pa pc pb ++ [(pa, pb)] ++ hanoi (n-1) pc pb pa

tests :: [Test]
tests = [ testEx1
        , testEx2
        , "testSum" ~: sumDigits [16,7,12,5] ~=? 1 + 6 + 7 + 1 + 2 + 5
        , testEx4
        , "testHanot" ~: hanoi 2 "a" "b" "c" ~=? [("a","c"), ("a","b"), ("c","b")]
        ]

main :: IO ()
main = runTestTT (TestList tests) >> return ()