{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module DateImprecise.T.Month
  ( tests )
where

-- base --------------------------------

import Data.Bool      ( Bool( False, True ), not )
import Data.Function  ( ($) )
import Data.Maybe     ( Maybe( Just ) )
import Data.String    ( String )
import System.Exit    ( ExitCode )
import System.IO      ( IO )

-- boundedn ----------------------------

import FromI  ( fromI' )

-- more-unicode ------------------------

import Data.MoreUnicode.Natural  ( ℕ )
import Data.MoreUnicode.Tasty    ( (≟) )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( assertBool, testCase )

-- tasty-plus --------------------------

import TastyPlus  ( runTestsP, runTestsReplay, runTestTree )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import DateImprecise.Month  ( month )

--------------------------------------------------------------------------------

monthTests ∷ TestTree
monthTests =
  let t i = case i of
              [month|7|] → True
              _          → False
      bool s x = testCase s $ assertBool s x
  in testGroup "month" [ testCase "Month 7" $ fromI' 7 ≟ Just [month|7|]
                       , bool "pattern Month 7" (t [month|7|])
                       , bool "pattern ! Month 6" (not $ t [month|6|])
                       ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "MonthQQ" [ monthTests ]
                
----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
