module Exercises where

import Test.HUnit

{- Read [instructions.md] first. -}

-- Exercise 1

prog :: IO Int
prog = do
  putStr "Say something: "
  input <- getLine
  let n = length input
  print (n * n)
  return n

prog' :: IO ()
prog' =
  putStr "Say something: " >>
  getLine >>=
  (\input -> print (length input * length input))

-- Exercise 2

q1 :: Bool
q1 = True

q2 :: Bool
q2 = False

q3 :: Bool
q3 = True

-- Exercise 3

lengthFile :: FilePath -> IO Int
lengthFile path = do
  content <- readFile path
  return (length content)

exercise3a :: Test
exercise3a =
  "lengthFile"
    ~: [ testLength "three/input1.txt" 20,
         testLength "three/input2.txt" 0,
         testLength "three/input3.txt" 11
       ]

concatFiles :: [FilePath] -> FilePath -> IO ()
concatFiles inputs output = do
  contents <- mapM readFile inputs
  writeFile output (concat contents)

exercise3b :: Test
exercise3b =
  "concatFiles"
    ~: [ testConcat
           ["three/input1.txt", "three/input2.txt", "three/input3.txt"]
           "three/result.txt"
           "three/expected.txt"
       ]

-- Exercise 4

-- putStr' = mapM_ putChar 没有显式参数是因为：
-- mapM_ putChar 本身已经是一个函数（类型为 String -> IO ()）
-- 这个函数可以直接赋值给 putStr'
-- 调用时传入的字符串参数会自动传递给内部的函数
-- 这是 Haskell 中常见的"点自由风格"编程方式

putStr' :: String -> IO ()
putStr' = mapM_ putChar

putStrLn' :: String -> IO ()
putStrLn' s = putStr' s >> putChar '\n'

print' :: Show a => a -> IO ()
print' = putStrLn' . show

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
        [ exercise3a,
          exercise3b,
          checkTime
        ]
  return ()

-- helper code for testing

testLength :: FilePath -> Int -> Assertion
testLength input n = do
  n' <- lengthFile input
  assertEqual "" n n'

testConcat :: [FilePath] -> FilePath -> FilePath -> Assertion
testConcat inputs output expected = do
  oute <- readFile expected
  concatFiles inputs output
  outr <- readFile output
  assertEqual "" oute outr