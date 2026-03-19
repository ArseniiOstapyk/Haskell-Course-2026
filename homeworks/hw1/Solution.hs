{-# LANGUAGE BangPatterns #-}

module Homework01 where

-- 1) Goldbach Pairs
goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n
  | n < 4 || odd n = []
  | otherwise =
      [ (p, q)
      | p <- [2 .. n `div` 2]
      , let q = n - p
      , isPrimeBounded p
      , isPrimeBounded q
      ]


-- 2) Coprime Pairs
coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs xs =
  [ (x, y)
  | x <- xs
  , y <- xs
  , x < y
  , gcd x y == 1
  ]


-- 3) Sieve of Eratosthenes
sieve :: [Int] -> [Int]
sieve []     = []
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primesTo :: Int -> [Int]
primesTo n = sieve [2 .. n]

isPrimeBounded :: Int -> Bool
isPrimeBounded n
  | n < 2     = False
  | otherwise = n `elem` primesTo n


-- 4. Matrix Multiplication
matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul a b =
  [ [ sum [ (a !! i !! k) * (b !! k !! j) | k <- [0 .. p - 1] ]
    | j <- [0 .. n - 1]
    ]
  | i <- [0 .. m - 1]
  ]
  where
    m = length a
    p = if null a then 0 else length (head a)
    n = if null b then 0 else length (head b)


-- 5) Permutations
permutations :: Int -> [a] -> [[a]]
permutations 0 _  = [[]]
permutations _ [] = []
permutations k xs =
  [ y : ys
  | (y, rest) <- picks xs
  , ys <- permutations (k - 1) rest
  ]

picks :: [a] -> [(a, [a])]
picks []     = []
picks (x:xs) = (x, xs) : [ (y, x:ys) | (y, ys) <- picks xs ]


-- 6) Hamming Numbers
-- a)
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | x > y     = y : merge (x:xs) ys
  | otherwise = x : merge xs ys

-- b)
hamming :: [Integer]
hamming = 1 : merge (map (2 *) hamming)
                    (merge (map (3 *) hamming)
                           (map (5 *) hamming))


-- 7) Integer Power with Bang Patterns
power :: Int -> Int -> Int
power b e
  | e < 0     = error "negative e"
  | otherwise = go 1 e
  where
    go !acc 0 = acc
    go !acc n = go (acc * b) (n - 1)


-- 8) Running Maximum: seq vs. Bang Patterns
-- Version 1
listMaxSeq :: [Int] -> Int
listMaxSeq []     = error "empty list"
listMaxSeq (x:xs) = go x xs
  where
    go acc []     = acc
    go acc (y:ys) =
      let acc' = max acc y
      in acc' `seq` go acc' ys

-- Version 2
listMaxBang :: [Int] -> Int
listMaxBang []     = error "empty list"
listMaxBang (x:xs) = go x xs
  where
    go !acc []     = acc
    go !acc (y:ys) = go (max acc y) ys


-- 9) Infinite Prime Stream
-- a) 
primes :: [Int]
primes = sieve [2..]

-- b)
isPrime :: Int -> Bool
isPrime n
  | n < 2     = False
  | otherwise = go primes
  where
    go (p:ps)
      | p * p > n      = True
      | n `mod` p == 0 = False
      | otherwise      = go ps
    go [] = True 


-- 10) Strict Accumulation and Space Leaks
-- a)
mean :: [Double] -> Double
mean xs =
  let (s, n) = go xs (0, 0)
  in if n == 0
       then error "empty list"
       else s / fromIntegral n
  where
    go []     (s, n) = (s, n)
    go (y:ys) (s, n) = go ys (s + y, n + 1)

-- b)
meanStrict :: [Double] -> Double
meanStrict xs =
  let (s, n) = go xs 0 0
  in if n == 0
       then error "empty list"
       else s / fromIntegral n
  where
    go []     !s !n = (s, n)
    go (y:ys) !s !n = go ys (s + y) (n + 1)

-- c)
meanVariance :: [Double] -> (Double, Double)
meanVariance xs =
  let (s, s2, n) = go xs 0 0 0
  in if n == 0
       then error "empty list"
       else
         let mu  = s / fromIntegral n
             var = s2 / fromIntegral n - mu * mu
         in (mu, var)
  where
    go []     !s !s2 !n = (s, s2, n)
    go (y:ys) !s !s2 !n = go ys (s + y) (s2 + y * y) (n + 1)