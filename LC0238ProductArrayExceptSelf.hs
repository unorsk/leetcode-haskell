module ProductExceptSelf where

productExceptSelf :: [Int] -> [Int]
productExceptSelf a =
  let prefix = 1 : scanl (*) 1 (init a)
      postfix = (reverse $ init $ scanl (*) 1 $ reverse a) ++ [1]
      z = zip prefix postfix in
        map (\(l,r) -> l*r) z

-- main :: IO ()
-- main = putStrLn $ show $ productExceptSelf [-1,1,0,-3,3]
