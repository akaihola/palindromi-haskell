module WordTools where

import Data.Char

isNotAlpha = not . isAlpha
hasAlpha = any isAlpha
isSameLetter x y = (toLower x) == (toLower y)

nonPunctuationError xs = error (show xs ++ " is not all punctuation")
nonLetterError x = error (show x ++ " is not a lower-case letter")
letterMismatchError x y = error (show x ++ " is not the same letter as " ++ show y)

takeUntil p [] = []
takeUntil p (x:[]) = [x]
takeUntil p (x:xs) = if p x then [x] else x : (takeUntil p xs)

tailFrom p = reverse . (takeUntil p) . reverse

