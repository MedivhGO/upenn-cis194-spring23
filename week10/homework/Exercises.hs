{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Exercises where

import BST
import Test.QuickCheck

{- Read [instructions.md] first. -}

-- Exercise 1

instance Arbitrary Tree where
  arbitrary :: Gen Tree
  arbitrary = sized (\n -> genTree (-n) n)
    where
      genTree :: Int -> Int -> Gen Tree
      genTree lower upper
        | lower > upper = return Leaf
        | otherwise =
          frequency
          [
            (1, return Leaf),
            (3, genBranch lower upper)
          ]
      genBranch lower upper = do
        x <- choose (lower, upper)
        left <- genTree lower (x - 1)
        right <- genTree (x + 1) upper
        return  (Branch left x right)

prop_ArbitraryValid :: Tree -> Bool
prop_ArbitraryValid = isBST

-- Exercise 2

prop_FindPostPresent :: Int -> Tree -> Bool
prop_FindPostPresent x t = find x (insert x t) == True

prop_FindPostAbsent :: Int -> Tree -> Bool
prop_FindPostAbsent x t = find x (delete x t) == False

---- end of exercises ----

{- Write down the approximate number of hours
it took you to complete this homework. If you have any
comments, feel free to also write them here. -}

time :: Double
time = 30