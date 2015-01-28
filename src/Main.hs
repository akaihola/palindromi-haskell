module Main where

import Criterion.Main

backwards :: [a] -> [a]
backwards l = rev l []
    where
        rev [] a = a
        rev (x:xs) a = rev xs (x:a)

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome l = isp l []
    where
        isp [] [] = True
        isp [_] [] = True
        isp [] _ = False
        isp (x:xs) [] = isp xs [x]
        isp (x:xs) (y:ys)
            | (x:xs) == (y:ys) = True  -- halfs are reverse of each other
            | xs     == (y:ys) = True  -- odd number of items
            | otherwise        = isp xs (x:y:ys)

--main :: IO ()
--main = do
--    print (isPalindrome "abcda")
--    print (isPalindrome "")
--    print (isPalindrome "a")
--    print (isPalindrome "aa")
--    print (isPalindrome "ab")
--    print (isPalindrome "aba")
--    print (isPalindrome "abc")

main :: IO ()
main = defaultMain [
       bgroup "fib" [ bench "10" $ whnf isPalindrome ""
                    , bench "35" $ whnf isPalindrome "a"
                    , bench "37" $ whnf isPalindrome "aa"
                    ]
                   ]