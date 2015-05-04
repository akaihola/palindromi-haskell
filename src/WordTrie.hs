module WordTrie (getLongerMatches,
                 getShorterMatches,
                 readTrieFromFile,
                 readTrieFromStdin,
                 Wordbook) where

import Prelude hiding (filter)
import Data.List (isPrefixOf)
import Data.ListTrie.Patricia.Set (empty, filter, insert, lookupPrefix,
                                   toList, TrieSet)
import Data.Map (Map)
import GHC.IO.Handle.Types (Handle)
import System.IO (hGetLine, hIsEOF, openFile, IOMode(ReadMode), stdin)


type Wordbook = TrieSet Map Char
readIntoTrie :: Handle -> Wordbook -> IO Wordbook
readIntoTrie handle t =
  do
    eof <- hIsEOF handle
    if eof
      then return t
      else do line <- hGetLine handle
              (readIntoTrie handle) $! insert line t

readTrieFromStdin :: IO (Wordbook)
readTrieFromStdin = readIntoTrie stdin empty


readTrieFromFile :: String -> IO (Wordbook)
readTrieFromFile path = do
  handle <- openFile path ReadMode
  readIntoTrie handle empty


type Match = (String, String)

lookupIsPrefixTo :: String -> Wordbook -> Wordbook
lookupIsPrefixTo word = filter ((flip isPrefixOf) word)

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
