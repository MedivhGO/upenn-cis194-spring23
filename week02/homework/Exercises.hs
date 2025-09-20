module Exercises where

import Test.HUnit
  ( Test (..),
    assertBool,
    runTestTT,
    (~:),
    (~?=),
  )
import Prelude hiding (even, max)

{- Read [instructions.md] first. -}

-- Exercise 1:

data Nat
  = Z
  | S Nat
  deriving (Show, Eq)

even :: Nat -> Bool
even Z = True
even (S Z) = False
even (S (S n)) = even n

exercise1a :: Test
exercise1a =
  "even"
    ~: [ even Z ~?= True,
         even (S Z) ~?= False,
         even (S (S Z)) ~?= True,
         even (S (S (S Z))) ~?= False
       ]

max :: Nat -> Nat -> Nat
max Z n = n
max n Z = n
max (S n) (S m) = S (max n m)

exercise1b :: Test
exercise1b =
  "max"
    ~: [ max Z Z ~?= Z,
         max (S (S Z)) (S (S (S Z))) ~?= S (S (S Z)),
         max (S (S (S Z))) (S (S Z)) ~?= S (S (S Z))
       ]

-- Exercise 2:

data Arith
  = Num Int
  | Add Arith Arith
  | Mul Arith Arith
  deriving (Show, Eq)

eval :: Arith -> Int
eval = error "unimplemented"

exercise2a :: Test
exercise2a =
  "eval"
    ~: [ eval (Num 3) ~?= 3,
         eval (Add (Num 4) (Num 5)) ~?= 9,
         eval (Mul (Add (Num 0) (Num 1)) (Mul (Num 2) (Num 3))) ~?= 6
       ]

opt0 :: Arith -> Arith
opt0 = error "unimplemented"

exercise2b :: Test
exercise2b =
  "opt0"
    ~: [ opt0 (Add (Num 3) (Num 2)) ~?= Add (Num 3) (Num 2),
         opt0 (Add (Num 0) (Num 7)) ~?= Num 7,
         opt0 (Mul (Num 2) (Add (Num 0) (Add (Num 0) (Num 1))))
           ~?= Mul (Num 2) (Num 1)
       ]

-- Exercise 3:

data Empty

{- Question:
  Is it possible to write a function of type Empty -> Int?
  If so, write one! If not, briefly explain why. -}

{- Question:
  Is it possible to write a function of type Int -> Empty?
  If so, write one! If not, briefly explain why. -}

---- end of exercises ----

{- Write down the approximate number of hours
it took you to complete this homework. If you have any
comments, feel free to also write them here. -}

time :: Double
time = error "unimplemented"

checkTime :: Test
checkTime = TestCase (assertBool "fill in any time" (time >= 0))

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestList
        [ exercise1a,
          exercise1b,
          exercise2a,
          exercise2b,
          checkTime
        ]
  return ()
