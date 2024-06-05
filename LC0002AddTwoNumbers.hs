module AddTwoNumbers where

import GHC.Base (List)
import Data.Foldable (Foldable(fold))
import Data.List (mapAccumL)

zipWithPad :: a -> b -> [a] -> [b] -> [(a, b)]
zipWithPad defaultA defaultB xs ys = go xs ys
  where
    go [] []         = []
    go (x:xs) []     = (x, defaultB) : go xs []
    go [] (y:ys)     = (defaultA, y) : go [] ys
    go (x:xs) (y:ys) = (x, y) : go xs ys

sumTwoDigits :: Int -> (Int, Int) -> (Int, Int)
sumTwoDigits accum (a, b) =
  let r = a + b + accum in
    if r <= 9 then (0, r) else (1, r - 10)

addTwoNumbers :: List Int -> List Int -> List Int
addTwoNumbers l1 l2 = 
  let (accum, r) = mapAccumL sumTwoDigits 0 (zipWithPad 0 0 l1 l2) in
    if accum == 0 then r else r ++ [accum]

main :: IO ()
main = do
  print $ addTwoNumbers [0] [0]
  print $ addTwoNumbers [2, 4, 3] [5, 6, 4]
  print $ addTwoNumbers [9,9,9,9,9,9,9] [9, 9, 9, 9]