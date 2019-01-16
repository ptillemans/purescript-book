module Count where

import Prelude

import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)

count:: forall a. (a -> Boolean) -> Array a -> Int
count p xs = count' 0 p xs
  where
    count' acc _ [] = acc
    count' acc p xs = let
       new_acc = if p (unsafePartial head xs)
                 then (acc + 1)
                 else acc
       rest = unsafePartial tail xs
     in count' new_acc p rest
