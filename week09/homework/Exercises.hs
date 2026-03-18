module Exercises where

import Control.Monad (guard)
import Data.List (sort, stripPrefix)
import Data.Maybe (isJust)
import Test.HUnit
import Text.Read (readMaybe)

{- Read [instructions.md] first. -}

-- Exercise 1

isGood :: String -> Bool
isGood = isJust . go
  where
    -- go evaluates to (Just "") on success, or Nothing otherwise
    go :: String -> Maybe String
    go "" = return ""
    go (c : rest) = do
      n <- readMaybe [c]
      guard (n >= 0)
      afterAs <- stripPrefix (replicate n 'a') rest
      go afterAs

exercise1 :: Test
exercise1 =
  "isGood"
    ~: [ all isGood ["", "3aaa2aa", "9aaaaaaaaa", "0", "001a", "2aa2aa"] ~?= True,
         not (any isGood ["3aaa2a", "4aaa", "1", "100a", "2bb2bb"]) ~?= True
       ]

-- Exercise 2

sumsTo :: Int -> [(Int, Int)]
sumsTo n = do
  x <- [0 .. n]
  y <- [0 .. n]
  guard (x + y == n)
  return (x, y)

sumsTo' :: Int -> [(Int, Int)]
sumsTo' n = [(x, y) | x <- [0 .. n], y <- [0 .. n], x + y == n]

exercise2 :: Test
exercise2 =
  "sumsTo"
    ~: [ sameElems (sumsTo 3) threes ~?= True,
         sameElems (sumsTo' 3) threes ~?= True
       ]
  where
    threes = [(0, 3), (1, 2), (2, 1), (3, 0)]

-- Exercise 3

join :: Monad m => m (m a) -> m a
join mmx = do
  mx <- mmx
  mx

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f mx = do
  x <- mx
  return (f x)

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f >=> g = \x -> do
  y <- f x
  g y

---- end of exercises ----

{- Write down the approximate number of hours
it took you to complete this homework. If you have any
comments, feel free to also write them here. -}

time :: Double
time = 30

checkTime :: Test
checkTime = TestCase (assertBool "fill in any time" (time >= 0))

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestList
        [ exercise1,
          exercise2,
          checkTime
        ]
  return ()

-- helper functions for tests

sameElems :: Ord a => [a] -> [a] -> Bool
sameElems xs ys = sort xs == sort ys