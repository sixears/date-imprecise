{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module DateImprecise.T.Tests
  ( tests )
where

-- base --------------------------------

import Data.String  ( String )
import System.Exit  ( ExitCode )
import System.IO    ( IO )

-- more-unicode ------------------------

import Data.MoreUnicode.Natural  ( ℕ )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-plus --------------------------

import TastyPlus  ( runTestsP, runTestsReplay, runTestTree )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  DateImprecise.DayOfM
import qualified  DateImprecise.T.DayOfM
import qualified  DateImprecise.Month
import qualified  DateImprecise.T.Month
import qualified  DateImprecise.Year
import qualified  DateImprecise.T.Year
import qualified  DateImprecise.DateImprecise
import qualified  DateImprecise.T.DateImprecise
import qualified  DateImprecise.DateImpreciseType
import qualified  DateImprecise.DateImpreciseRange
import qualified  DateImprecise.T.DateImpreciseRange

--------------------------------------------------------------------------------

tests ∷ TestTree
tests =
  testGroup "MInfo" [ DateImprecise.DayOfM.tests
                    , DateImprecise.T.DayOfM.tests
                    , DateImprecise.Month.tests
                    , DateImprecise.T.Month.tests
                    , DateImprecise.Year.tests
                    , DateImprecise.T.Year.tests
                    , DateImprecise.DateImprecise.tests
                    , DateImprecise.T.DateImprecise.tests
                    , DateImprecise.DateImpreciseType.tests
                    , DateImprecise.DateImpreciseRange.tests
                    , DateImprecise.T.DateImpreciseRange.tests
                    ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
