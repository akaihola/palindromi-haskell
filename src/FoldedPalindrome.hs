module FoldedPalindrome where

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

-- data Fragment = Fragment String String Char
--               deriving (Show)
-- fragment front back letter | not (isLower letter) = nonLetterError letter
--                            | hasAlpha (init front) = nonPunctuationError (init front)
--                            | hasAlpha (tail back) = nonPunctuationError (tail back)
--                            | toLower (last front) /= letter = letterMismatchError (last front) letter
--                            | toLower (head back) /= letter = letterMismatchError (head back) letter
--                            | otherwise = Fragment front back letter
-- center front back Nothing | hasAlpha front = nonPunctuationError front
--                           | hasAlpha back = nonPunctuationError back
--                           | otherwise = Center front back Nothing
-- center front back (Just letter) | hasAlpha front = nonPunctuationError front
--                                 | hasAlpha (tail back) = nonPunctuationError (tail back)
--                                 | toLower (last front) /= letter = letterMismatchError (last front) letter
--                                 | otherwise = Center front back (Just letter)

-- Piece                                         Center
-- ["A] [l] [l] [a] [s] [i] [ o] [n] [ a] [s]    [e]
-- [a.] [l] [l] [a] [S ] [i] [ o] [n] [ a] [s]   [ ,"]
data Story = Center String String (Maybe Char) | Piece String String Char Story
           deriving (Show)

extend :: String -> Story -> Story
extend "" xs = xs

-- story :: String -> Story
-- story "" = StoryCenter "" "" Nothing
-- -- "A" StoryCenter "" "a" (Just "a")
-- -- "." StoryCenter "." "" Nothing
-- story (x:"") | isAlpha x = StoryCenter "" [x] (Just (toLower x))
--              | otherwise = StoryCenter [x] "" Nothing
-- story xs | null front = StoryCenter xs "" Nothing
--          | otherwise = fragment front back 'x' :-> story ""
--   where front = takeUntil isAlpha xs
--         back = tailFrom isAlpha front

-- split :: String -> String -> String -> (String, String, String)
-- split xs "" zs = (xs, "", zs)
-- split xs ys (z:zs) | isAlpha (last xs) && isAlpha z = (xs, ys, (z:zs))
-- split xs (y:ys) zs | isNotAlpha (last xs) = split (xs ++ [y]) ys zs
-- split xs ys (z:zs) | isNotAlpha z = split xs (init ys) ((last ys) : (z:zs))

-- (+++) :: Story -> Story -> Story
-- Center xs +++ Center ys | hasAlpha xs = nonPunctuationError xs
--                     | hasAlpha ys = nonPunctuationError ys
--                     | otherwise = Center (xs ++ ys)
-- Center xs +++ (Fragment ys y :-> z) | hasAlpha xs = nonPunctuationError xs
--                                   | hasAlpha ys = nonPunctuationError ys
--                                   | isNotAlpha y = nonLetterError y
--                                   | otherwise = (Fragment (xs ++ ys) y) :-> z
-- (x :-> y) +++ z = x :-> (y +++ z)

-- reverseStory l = rev l (Center "")
--   where
--     rev (Fragment xs x :-> ys) (Center "") = rev (Fragment "" x :-> ys) (Center (reverse xs))
--     rev (Fragment xs x :-> ys) (Fragment "" a :-> b) = rev (Fragment "" x :-> ys) (Fragment (reverse xs) a :-> b)
--     rev (x :-> ys) a = rev ys (x :-> a)
--     rev (Center xs) (Fragment "" a :-> b) = rev (Center "") (Fragment (reverse xs) a :-> b)
--     rev (Center "") a = a

-- toString :: Story -> String
-- toString (Center xs) = xs
-- toString (Fragment xs x :-> y) = xs ++ [x] ++ (toString y)

-- symbols :: Story -> String
-- symbols (Center xs) = ""
-- symbols (Fragment xs x :-> y) = (toLower x):(symbols y)

