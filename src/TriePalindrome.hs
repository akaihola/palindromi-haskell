module TriePalindrome (main, readTrieFromFile) where

import Data.ListTrie.Patricia.Set (empty, insert, lookupPrefix, toList, TrieSet)
import qualified Data.ListTrie.Patricia.Set as S (filter, map)
import Data.List (intercalate, isPrefixOf)
import Data.Map (Map)
import GHC.IO.Handle.Types (Handle)

import System.IO          (hGetLine, hIsEOF, openFile, IOMode(ReadMode), stdin)
import System.Environment (getArgs)

import Debug.Trace (trace)


type Wordbook = TrieSet Map Char
data MirrorDictionaries = MirrorDictionaries Wordbook Wordbook

mirrorDictionaries :: Wordbook -> MirrorDictionaries
mirrorDictionaries words = MirrorDictionaries words (S.map reverse words)

swap :: MirrorDictionaries -> MirrorDictionaries
swap (MirrorDictionaries forward mirror) = MirrorDictionaries mirror forward


readIntoTrie :: Handle -> Int -> Wordbook -> IO Wordbook
readIntoTrie handle linenum t = do eof <- hIsEOF handle
                                   if eof then return t
                                   else do line <- hGetLine handle
                                           (readIntoTrie handle $! linenum + 1) $! insert line t

readTrieFromStdin :: IO (Wordbook)
readTrieFromStdin = do readIntoTrie stdin 0 empty >>= return


readTrieFromFile :: String -> IO (Wordbook)
readTrieFromFile path = do
  handle <- openFile path ReadMode
  t <- readIntoTrie handle 0 empty
  return t


isPalindrome :: String -> Bool
isPalindrome bs = reverse bs == bs


type Match = (String, String)
type Palindrome = [String]

lookupIsPrefixTo :: String -> Wordbook -> Wordbook
lookupIsPrefixTo word = S.filter ((flip isPrefixOf) word)

getLongerMatches :: String -> Wordbook -> [Match]
getLongerMatches prefix dictionary = [(match, drop (length prefix) match)
                                     | match <- toList (lookupPrefix prefix dictionary),
                                                match /= prefix]

getShorterMatches :: String -> Wordbook -> [Match]
getShorterMatches prefix dictionary = [(match, drop (length match) prefix)
                                      | match <- toList (lookupIsPrefixTo prefix dictionary)]


-- Filter out palindromes whose first word has already been used in the rest of the palindrome
filterUsed :: [[String]] -> [[String]]
filterUsed = filter (\(x:xs) -> not (elem x xs))

findPalindromes :: Int -> String -> MirrorDictionaries -> [Palindrome]
findPalindromes 0 _ _ = []
findPalindromes n prefix dictionaries
    = final ++ longer ++ shorter
    where final :: [Palindrome]
          final | isPalindrome prefix = [[]]
                | otherwise = []
          (MirrorDictionaries dictionary _) = dictionaries
          longerMatches = getLongerMatches prefix dictionary
          shorterMatches = getShorterMatches prefix dictionary
          allLonger = [(match:rest)
                        | (match, overflow) <- longerMatches,
                          restBackwards <- findPalindromes (n - 1) overflow (swap dictionaries),
                          let rest = reverse $ map reverse restBackwards]
          longer = filterUsed allLonger
          allShorter = [(match:rest)
                        | (match, underflow) <- shorterMatches,
                          rest <- findPalindromes (n - 1) underflow dictionaries]
          shorter = filterUsed allShorter


main :: IO ()
main = do
    [maxLength] <- getArgs
    wordsL <- readTrieFromStdin
    let dictionaries = mirrorDictionaries wordsL
    let palindromes = findPalindromes (1 + (read maxLength :: Int)) "" dictionaries
    -- mapM_ putStrLn (toList wordsL)
    -- putStrLn (show wordsL)
    mapM_ (putStrLn . (intercalate " ")) palindromes
    -- mapM_ (putStrLn . show) palindromes
    return ()
