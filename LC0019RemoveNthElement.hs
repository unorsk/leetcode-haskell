module RemoveNthElement where

data MyList a = Empty | Node a (MyList a) deriving (Show)

fromList :: [a] -> MyList a
fromList [] = Empty
fromList (x : xs) = Node x (fromList xs)

removeNthElement :: MyList Int -> Int -> MyList Int
removeNthElement l n
  | n > 0 = removeNthElement l (n - 1)
  | otherwise = undefined

main :: IO ()
main =
  print $ fromList [1, 2, 3, 4]