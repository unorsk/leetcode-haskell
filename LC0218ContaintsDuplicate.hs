module ContaintsDuplicate where

import Data.Set

findDuplicates :: [Int] -> Bool
findDuplicates a =
  let s = empty in
    fd a s
    where 
      fd :: [Int] -> Set Int -> Bool
      fd [] _ = False
      fd (a1:ar) s1 =
        if (member a1 s1)
          then True
          else fd ar (insert a1 s1)

main :: IO ()
main = print $ findDuplicates [1, 2, 3, 4, 5, 1]
