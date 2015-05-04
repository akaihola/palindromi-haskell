module WordTrie (getLongerMatches,
                 getShorterMatches,
                 readTrieFromFile,
                 readTrieFromStdin,
                 Wordbook) where

import Data.Char (isAlpha, toLower)
import Data.List (isPrefixOf)
import Data.ListTrie.Patricia.Set (empty, insert, lookupPrefix,
                                   toList, TrieSet)
import qualified Data.Text as T (dropWhile, dropWhileEnd, filter, pack, Text, toLower, unpack)
import qualified Data.ListTrie.Patricia.Set as Set (filter)
import Data.Map (Map)
import GHC.IO.Handle.Types (Handle)
import System.IO (hGetContents, hGetLine, hIsEOF, openFile, IOMode(ReadMode), stdin)


type Wordbook = TrieSet Map Char
readIntoTrie :: [String] -> Wordbook -> IO Wordbook
readIntoTrie [] wordbook = do
  return wordbook
readIntoTrie (x:xs) wordbook | null word = readIntoTrie xs wordbook
                             | otherwise = readIntoTrie xs $ insert word wordbook
    where word = normalizeWord x

readTrieFromStdin :: IO (Wordbook)
readTrieFromStdin = do
  content <- hGetContents stdin
  let tokens = words content
  readIntoTrie tokens empty


strip :: T.Text -> T.Text
strip = T.dropWhileEnd (not . isAlpha) . T.dropWhile (not . isAlpha)
clean :: T.Text -> T.Text
clean = T.filter isAlpha

normalizeWord = T.unpack . clean . T.toLower . strip . T.pack

readTrieFromFile :: String -> IO (Wordbook)
readTrieFromFile path = do
  handle <- openFile path ReadMode
  content <- hGetContents handle
  let tokens = words content
  readIntoTrie tokens empty


type Match = (String, String)

lookupIsPrefixTo :: String -> Wordbook -> Wordbook
lookupIsPrefixTo word = Set.filter ((flip isPrefixOf) word)

getLongerMatches :: String -> Wordbook -> [Match]
getLongerMatches prefix dictionary =
  [(match, drop (length prefix) match)
  | match <- toList (lookupPrefix prefix dictionary)
  , match /= prefix]

getShorterMatches :: String -> Wordbook -> [Match]
getShorterMatches prefix dictionary =
  [(match, drop (length match) prefix)
  | match <- toList (lookupIsPrefixTo prefix dictionary)]



main :: IO ()
main = do
  p <- readTrieFromStdin
  mapM_ putStrLn (toList p)
