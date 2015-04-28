module TriePalindrome (main, readTrieFromFile) where

import Prelude hiding (filter)
import Data.ListTrie.Patricia.Set (empty, filter, insert, lookupPrefix, toList, TrieSet)
import qualified Data.ListTrie.Patricia.Set as S (map)
import Data.List (intercalate, isPrefixOf)
import Data.Map (Map)
import GHC.IO.Handle.Types (Handle)

import System.IO          (hGetLine, hIsEOF, openFile, IOMode(ReadMode), stdin)
import System.Environment (getArgs)

import Debug.Trace (trace)


type Wordbook = TrieSet Map Char

readIntoTrie :: Handle -> Int -> Wordbook -> IO Wordbook
readIntoTrie handle linenum t = do eof <- hIsEOF handle
                                   if eof then return t
                                   else do line <- hGetLine handle
                                           (readIntoTrie handle $! linenum + 1) $! insert line t

readTrieFromStdin :: IO (Wordbook)
readTrieFromStdin =
  readIntoTrie handle 0 empty >>= return
      where handle = stdin


readTrieFromFile :: String -> IO (Wordbook)
readTrieFromFile path = do
  handle <- openFile path ReadMode
  t <- readIntoTrie handle 0 empty
  return t


isPalindrome :: String -> Bool
isPalindrome bs = reverse bs == bs


type Match = (String, String)
type Palindrome = [String]

getLongerMatches :: String -> Wordbook -> [Match]
getLongerMatches prefix dictionary = [(match, drop (length prefix) match)
                                     | match <- toList (lookupPrefix prefix dictionary),
                                                match /= prefix]

getShorterMatches :: String -> Wordbook -> [Match]
getShorterMatches prefix dictionary = [(match, drop (length match) prefix)
                                      | match <- toList (filter ((flip isPrefixOf) prefix) dictionary)]

findPalindromes :: Int -> String -> Wordbook -> Wordbook -> [Palindrome]
findPalindromes 0 _ _ _ = []
findPalindromes n prefix dictionary reverseDictionary 
    = final ++ longer ++ shorter
    where final :: [Palindrome]
          final | isPalindrome prefix = [[]]
                | otherwise = []
          longerMatches = getLongerMatches prefix dictionary
          shorterMatches = getShorterMatches prefix dictionary
          longer = [(match:(reverse $ map reverse restBackwards))
                        | (match, overflow) <- longerMatches,
                          restBackwards <- findPalindromes (n - 1) overflow reverseDictionary dictionary,
                          not (elem match (reverse $ map reverse restBackwards))]
          shorter = [(match:rest)
                        | (match, underflow) <- shorterMatches,
                          rest <- findPalindromes (n - 1) underflow dictionary reverseDictionary,
                          not (elem match rest)]


main :: IO ()
main = do
    [maxLength] <- getArgs
    wordsL <- readTrieFromStdin
    let wordsR = S.map reverse wordsL
    let palindromes = findPalindromes (1 + (read maxLength :: Int)) "" wordsL wordsR
    -- mapM_ putStrLn (toList wordsL)
    -- putStrLn (show wordsL)
    mapM_ (putStrLn . (intercalate " ")) palindromes
    -- mapM_ (putStrLn . show) palindromes
    return ()
