{-# LANGUAGE TemplateHaskell #-}
module FindPalindromesTests
where

import Test.Framework.TH (defaultMainGenerator)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?))

import FindPalindromes (findPairs)

main = $(defaultMainGenerator)

case_findPairs_empty = do [] @=? findPairs ([] :: [Int])
case_findPairs_one   = do [] @=? findPairs [5]
case_findPairs_two   = do [(1, 2)] @=? findPairs [1, 2]
case_findPairs_three = do [(1, 2), (1, 3), (2, 3)] @=? findPairs [1, 2, 3]
case_findPairs_four  = do [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)] @=? findPairs [1..4]
case_findPairs_ten  = do 4950
