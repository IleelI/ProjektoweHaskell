module Main where

{- Dla danej liczby naturalnej n podaj taką liczbę `x <= n`, którą da się rozłożyć
 na największą liczbę różnych trójek `a, b, c`, takich, że `a + b + c = x`
oraz z boków o długości `a, b, c` można zbudować trójkąt prostokątny. -}

isRightTriangle :: (Integral n, Eq n) => n -> n -> n -> Bool
isRightTriangle a b c =
  a ^ 2 + b ^ 2 == c ^ 2
    || c ^ 2 + b ^ 2 == a ^ 2
    || a ^ 2 + c ^ 2 == b ^ 2

isSumOfNumber :: (Integral n, Eq n) => n -> n -> n -> n -> Bool
isSumOfNumber a b c number = a + b + c == number

areDifferentTriples :: (Integral n) => n -> n -> n -> Bool
areDifferentTriples a b c = a /= b && a /= c && b /= c

getTriplesOfNumber :: (Integral n) => n -> [(n, n, n)]
getTriplesOfNumber number =
  [ (a, b, c)
    | a <- [1 .. number],
      b <- [1 .. number],
      c <- [1 .. number],
      isRightTriangle a b c
        && isSumOfNumber a b c number
        && areDifferentTriples a b c
  ]

getAllNumbers :: (Integral n) => n -> [(Int, n)]
getAllNumbers number = [(length (getTriplesOfNumber n), n) | n <- [1 .. number]]

main :: IO ()
main = do
  putStrLn "Enter any natural number: "
  input <- getLine
  let results = getAllNumbers (read input :: Int)
  let maxNumber = maximum results
  print (snd maxNumber)