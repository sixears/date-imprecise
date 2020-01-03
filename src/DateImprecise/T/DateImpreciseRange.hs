{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module DateImprecise.T.DateImpreciseRange
  ( tests )
where

-- base --------------------------------

import Data.Either    ( Either( Right ) )
import Data.Function  ( ($) )
import Data.String    ( String )
import System.Exit    ( ExitCode )
import System.IO      ( IO )

-- more-unicode ------------------------

import Data.MoreUnicode.Natural  ( ℕ )
import Data.MoreUnicode.Tasty    ( (≟) )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-plus --------------------------

import TastyPlus  ( runTestsP, runTestsReplay, runTestTree )

-- time --------------------------------

import Data.Time  ( fromGregorian )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import DateImprecise.DateImpreciseRange
                              ( dateImpreciseR', dateImpreciseRange )
import DateImprecise.DateImprecise
                              ( dayDate, pattern MonthDate, pattern YearDate )
import DateImprecise.Month    ( month )
import DateImprecise.Year     ( year )

--------------------------------------------------------------------------------

qqTests ∷ TestTree
qqTests =
  let d0s = YearDate [year|2019|]
      d0e = dayDate (fromGregorian 2019 12 24)
      d0 = dateImpreciseR' d0s d0e
      d1s = YearDate [year|2017|]
      d1e = MonthDate [year|2019|] [month|12|]
      d1 = dateImpreciseR' d1s d1e
      d2 = dateImpreciseR' d0s d0s
   in testGroup "dateImpreciseRangeQQ"
                [ testCase "DateImpreciseRange 2019-12-24" $
                    d0 ≟ Right [dateImpreciseRange|2019:12-24|]
                , testCase "DateImpreciseRange 2019-12" $
                    d1 ≟ Right [dateImpreciseRange|2017:2019-12|]
                , testCase "DateImpreciseRange 2019" $
                    d2 ≟ Right [dateImpreciseRange|2019|]
            ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "DateImpreciseRangeQQ" [ qqTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
