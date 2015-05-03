module FindPalindromes
where


-- | All possible ways to choose @k@ elements from a list, without
-- repetitions. \"Antisymmetric power\" for lists. Synonym for 'kSublists'.
choose :: Int -> [a] -> [[a]]
choose 0 _  = [[]]
choose k [] = []
choose k (x:xs) = map (x:) (choose (k-1) xs) ++ choose k xs


-- | \"Tensor power\" for lists. Special case of 'listTensor':
--
-- > tuplesFromList k xs == listTensor (replicate k xs)
-- 
-- See also "Math.Combinat.Tuples".
-- TODO: better name?
tuplesFromList :: Int -> [a] -> [[a]]
tuplesFromList 0 _  = [[]]
tuplesFromList k xs = [ (y:ys) | ys <- tuplesFromList (k-1) xs , y <- xs ]
--the order seems to be very important, the wrong order causes a memory leak!
--tuplesFromList k xs = [ (y:ys) | y <- xs, ys <- tuplesFromList (k-1) xs ]


combinations :: Int -> [a] -> [[a]]
combinations k xs = foldl1 (++) [choose n xs | n <- [1..k]]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs



main = do
  content <- readFile "10000pysty.txt"
  let words = lines content
  let combis = combinations 6 (take 50 words)
  let palindromes = filter (isPalindrome . concat) combis
  mapM (putStrLn . show) palindromes
  -- putStrLn (show combis)
  return ()
