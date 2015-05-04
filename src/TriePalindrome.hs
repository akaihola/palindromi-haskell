module TriePalindrome (main, readTrieFromFile) where

import WordTrie (getLongerMatches,
                 getShorterMatches,
                 readTrieFromFile,
                 readTrieFromStdin,
                 Wordbook)
import Data.ListTrie.Patricia.Set (toList)
import qualified Data.ListTrie.Patricia.Set as S (map)
import Data.List (intercalate)
import qualified Data.Set as Set (empty, insert, map, member, Set)

import System.Environment (getArgs)

import Debug.Trace (traceShow)


data MirrorDictionaries = MirrorDictionaries Wordbook Wordbook

mirrorDictionaries :: Wordbook -> MirrorDictionaries
mirrorDictionaries words = MirrorDictionaries words (S.map reverse words)

swap :: MirrorDictionaries -> MirrorDictionaries
swap (MirrorDictionaries forward mirror) = MirrorDictionaries mirror forward


isPalindrome :: String -> Bool
isPalindrome bs = reverse bs == bs


type Palindrome = [String]

-- Filter out palindromes whose first word has already been used in the rest of the palindrome
filterUsed :: [[String]] -> [[String]]
filterUsed = filter (\(x:xs) -> not (elem x xs))

findPalindromes :: Int -> String -> MirrorDictionaries -> Set.Set String -> [Palindrome]
findPalindromes 0 _ _ _ = []
findPalindromes n prefix dictionaries usedWords
    = final ++ longer ++ shorter
    where
      final :: [Palindrome]
      final | isPalindrome prefix = [[]]
            | otherwise = []
      (MirrorDictionaries dictionary _) = dictionaries
      longerMatches = getLongerMatches prefix dictionary
      shorterMatches = getShorterMatches prefix dictionary
      longer = [(match:rest)
               | (match, overflow) <- longerMatches
               , not (Set.member match usedWords)
               , restBackwards <- loop overflow (swap dictionaries) (Set.map reverse (Set.insert match usedWords))
               , let rest = reverse2d restBackwards]
      shorter = [(match:rest)
                | (match, underflow) <- shorterMatches
                , not (Set.member match usedWords)
                , rest <- loop underflow dictionaries (Set.insert match usedWords)]
      reverse2d = reverse . map reverse
      loop = findPalindromes (n - 1)


main :: IO ()
main = do
    [maxLength] <- getArgs
    wordsL <- readTrieFromStdin
    let dictionaries = mirrorDictionaries wordsL
    let palindromes = findPalindromes (1 + (read maxLength :: Int)) "" dictionaries Set.empty
    -- mapM_ putStrLn (toList wordsL)
    -- putStrLn (show wordsL)
    mapM_ (putStrLn . (intercalate " ")) palindromes
    -- mapM_ (putStrLn . show) palindromes
    return ()
