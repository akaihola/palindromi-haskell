import Storyfragment

data Change = Change Int Int String deriving (Show)

diffLeft [] _ offset = offset
diffLeft _ [] offset = offset
diffLeft (x:xs) (y:ys) offset
  | x == y = diffLeft xs ys (offset + 1)
  | otherwise = offset

diffSlice xs ys = Change left right replacement
  where left = diffLeft xs ys 0
        reverseRemaining count = reverse . drop count
        rxs = reverseRemaining left xs
        rys = reverseRemaining left ys
        right = diffLeft rxs rys 0
        replacement = take ((length ys) - left - right) $ drop left $ ys
        
        
-- storyDiff xs ys = Change left right replacement
--   where left = storyDiffLeft xs ys 0
--         reverseRemaining count = reverse . drop count
--         rxs = 
