{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module DateImprecise.T.Year
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

import DateImprecise.Year  ( year )

--------------------------------------------------------------------------------

yearTests ∷ TestTree
yearTests =
  let t i = case i of
              [year|2017|] → True
              _            → False
      bool s x = testCase s $ assertBool s x
  in testGroup "year" [ testCase "Year 2017" $ fromI' 2017 ≟ Just [year|2017|]
                      , bool "pattern Year 2017" (t [year|2017|])
                      , bool "pattern ! Year 2016" (not $ t [year|2016|])
                      ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "YearQQ" [ yearTests ]
                
----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
