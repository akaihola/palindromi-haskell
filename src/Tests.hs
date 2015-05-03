{-# LANGUAGE TemplateHaskell #-}
module Tests
where

import Data.Char (isAlpha)
import Test.Framework.TH (defaultMainGenerator)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?))

import WordTools (isNotAlpha, hasAlpha, takeUntil)

main = $(defaultMainGenerator)

case_isNotAlpha_1 = do False @=? isNotAlpha 'a'
case_isNotAlpha_2 = do False @=? isNotAlpha 'Z'
case_isNotAlpha_3 = do False @=? isNotAlpha 'ä'

case_isNotAlpha_4 = do True @=? isNotAlpha ' '
case_isNotAlpha_5 = do True @=? isNotAlpha '.'
case_isNotAlpha_6 = do True @=? isNotAlpha '5'

case_hasAlpha_1 = do False @=? hasAlpha ""
case_hasAlpha_2 = do False @=? hasAlpha " .'%9,_/"

case_hasAlpha_lower_a = do True @=? hasAlpha "!#¤%&a()"
case_hasAlpha_unicode = do True @=? hasAlpha "+-*/Ä,. "
case_hasAlpha_allalfa = do True @=? hasAlpha "AllAlpha"
                    
case_takeUntil_empty = do "" @=? takeUntil isAlpha ""
case_takeUntil_one = do "Z" @=? takeUntil isAlpha "Z"
case_takeUntil_one_false = do "." @=? takeUntil isAlpha "."
case_takeUntil_first = do "A" @=?takeUntil isAlpha "A--Z"
case_takeUntil_never = do "+-*/" @=? takeUntil isAlpha "+-*/"
