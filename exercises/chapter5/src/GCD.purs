module GCD where

import Data.Maybe (Maybe(..))
import Prelude


gcd'::Int -> Int -> Int
gcd' n 0 = n
gcd' 0 m = m
gcd' n m = if n > m
          then gcd' (n - m) m
          else gcd' n (m - n)


factorial'::Int -> Int
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)


pascal:: Int -> Int -> Maybe Int
pascal 1 _ = Just 1
pascal _ 1 = Just 1
pascal n k
  | k > n     = Nothing
  | k <= 0    = Nothing
  | n <= 0    = Nothing
  | n == k    = Just 1
  | otherwise = (+) <$> p1 <*> p2
  where
    p1 = pascal (n - 1) (k - 1)
    p2 = pascal (n - 1) k
