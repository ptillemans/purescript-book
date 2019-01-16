module Factorization where

import Prelude
import Control.MonadZero (guard)
import Data.Array ((..), (:))

factorizations :: Int -> Array (Array Int)
factorizations n
  | n < 4 = [[n]]
  | otherwise = do
    i <- 2 .. n `div` 2
    guard $ n `mod` i == 0 && i >= 2
    fs <- factorizations (n `div` i)
    pure (i : fs)
