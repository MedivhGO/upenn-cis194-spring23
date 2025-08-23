module Exercises where

import Test.HUnit
  ( Test (..),
    Testable (..),
    assertBool,
    runTestTT,
    (~:),
    (~?=),
  )

{- Read [instructions.md] first. -}

{-
Exercise 1: We first need to be able to break up a number into its last
digit and the rest of the number. Fill in the functions below:

Hint: Use `mod` for the first function and `div` for the second.
-}

lastDigit :: Int -> Int
lastDigit x = x `mod` 10

dropLastDigit :: Int -> Int
dropLastDigit x = x `div` 10

{- Here, we have some tests written using Haskell's unit testing library.
   For example, the first one says the result of (lastDigit 1234) should be 4.
-}
exercise1 :: Test
exercise1 =
  test
    [ "lastDigit"
        ~: [ lastDigit 1234 ~?= 4,
             lastDigit 10 ~?= 0,
             lastDigit 5 ~?= 5,
             lastDigit 0 ~?= 0
           ],
      "dropLastDigit"
        ~: [ dropLastDigit 1234 ~?= 123,
             dropLastDigit 10 ~?= 1,
             dropLastDigit 5 ~?= 0,
             dropLastDigit 0 ~?= 0
           ]
    ]

{-
Exercise 2: Now, we can break apart a number into its digits. It is
actually easier to break a number into a list of its digits in reverse
order (can you figure out why?). Fill in the function below:

For zero or negative inputs, toRevDigits should return the empty list.
-}

toRevDigits :: Int -> [Int]
toRevDigits x =  if x <= 0 then [] else lastDigit x : toRevDigits (dropLastDigit x)

exercise2 :: Test
exercise2 =
  "toRevDigits"
    ~: [ toRevDigits 1234 ~?= [4, 3, 2, 1],
         toRevDigits 5 ~?= [5],
         toRevDigits 0 ~?= [],
         toRevDigits (-17) ~?= []
       ]

{-
Exercise 3: Once we have the digits in the proper order, we need to
double every other one from left to right. Fill in the function below:
-}

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs) = x : (2 * y) : doubleEveryOther xs

exercise3 :: Test
exercise3 =
  "doubleEveryOther"
    ~: [ doubleEveryOther [] ~?= [],
         doubleEveryOther [1] ~?= [1],
         doubleEveryOther [1, 2] ~?= [1, 4],
         doubleEveryOther [1, 2, 3] ~?= [1, 4, 3],
         doubleEveryOther [1, 2, 3, 4] ~?= [1, 4, 3, 8]
       ]

{-
Exercise 4: Next, we will want to sum together the *digits*
in a list of integers. Fill in the function below:
-}

sumDigits :: [Int] -> Int
sumDigits (x:xs) = sum (toRevDigits x) + sumDigits xs
sumDigits [] = 0


exercise4 :: Test
exercise4 =
  "sumDigits"
    ~: [ sumDigits [5, 10, 4, 18] ~?= 5 + 1 + 0 + 4 + 1 + 8,
         sumDigits [] ~?= 0,
         sumDigits [123, 4] ~?= 1 + 2 + 3 + 4
       ]

{-
Exercise 5: We are now ready to determine whether a credit card number
is valid! Fill in the function below:

Remember, don't repeat yourself: you should use the functions
you defined in previous exercises.
-}

validate :: Int -> Bool
validate x = sumDigits (doubleEveryOther (toRevDigits x)) `mod` 10 == 0

exercise5 :: Test
exercise5 =
  "validate"
    ~: [ validate 5594589764218858 ~?= True,
         validate 1234567898765432 ~?= False
       ]

{-
Exercise 6: Write down the approximate number of hours
it took you to complete this homework. If you have any
comments, feel free to also write them here.
-}

time :: Double
time = 30

exercise6 :: Test
exercise6 = TestCase (assertBool "fill in any time" (time >= 0))

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestList
        [ exercise1,
          exercise2,
          exercise3,
          exercise4,
          exercise5,
          exercise6
        ]
  return ()
