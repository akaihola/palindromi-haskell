import Data.Char

isNotAlpha = not . isAlpha
hasAlpha = any isAlpha

nonPunctuationError xs = error (show xs ++ " is not all punctuation")
nonLetterError x = error (show x ++ " is not a letter")

data Fragment = Fragment String Char
              deriving (Show)
fragment punctuation letter | isNotAlpha letter = nonLetterError letter
                            | hasAlpha punctuation = nonPunctuationError punctuation
                            | otherwise = Fragment punctuation letter

data Story = Tail String | Fragment :-> Story
           deriving (Show)
infixr :->

story :: String -> Story
story "" = Tail ""
story (x:"") | isAlpha x = (Fragment "" x) :-> (Tail "")
             | otherwise = Tail [x]
story (x:xs) = story [x] +++ story xs

(+++) :: Story -> Story -> Story
Tail xs +++ Tail ys | hasAlpha xs = nonPunctuationError xs
                    | hasAlpha ys = nonPunctuationError ys
                    | otherwise = Tail (xs ++ ys)
Tail xs +++ (Fragment ys y :-> z) | hasAlpha xs = nonPunctuationError xs
                                  | hasAlpha ys = nonPunctuationError ys
                                  | isNotAlpha y = nonLetterError y
                                  | otherwise = (Fragment (xs ++ ys) y) :-> z
(x :-> y) +++ z = x :-> (y +++ z)

toString :: Story -> String
toString (Tail xs) = xs
toString (Fragment xs x :-> y) = xs ++ [x] ++ (toString y)

symbols :: Story -> String
symbols (Tail xs) = ""
symbols (Fragment xs x :-> y) = (toLower x):(symbols y)

