{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}

module DateImprecise.DateImpreciseType
  ( DateImprecise(..), cdayToDate, dateDay_, dateMonth, dateYear, endDateOfMonth
  , tests ) 
where

import Prelude  ( Float, Integer, Integral, toInteger )

-- aeson -------------------------------

import Data.Aeson.Types  ( typeMismatch )

-- base --------------------------------

import Control.Monad       ( fail, return )
import Data.Either         ( Either( Left, Right ) )
import Data.Eq             ( Eq )
import Data.Function       ( ($) )
import Data.List.NonEmpty  ( NonEmpty( (:|) ) )
import Data.Maybe          ( Maybe( Just, Nothing ) )
import Data.String         ( String )
import Data.Tuple          ( curry )
import System.Exit         ( ExitCode )
import System.IO           ( IO )
import Text.Show           ( Show )

-- base-unicode-functions --------------

import Data.Function.Unicode  ( (∘) )

-- boundedn ----------------------------

import FromI  ( fromI, __fromI )
import ToNum  ( ToNum( toNum, toNumW8, toNumW16 ) )

-- data-textual ------------------------

import Data.Textual  ( Parsed( Parsed, Malformed ), Printable( print )
                     , Textual( textual ), fromString, parseText , toText )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵), (⋪) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Natural      ( ℕ )
import Data.MoreUnicode.Tasty        ( (≟) )

-- parsec-plus -------------------------

import ParsecPlus  ( Parsecable( parser ), parsec' )

-- parser-plus -------------------------

import ParserPlus  ( tries )

-- parsers ------------------------------

import Text.Parser.Char  ( string )

-- scientific --------------------------

import Data.Scientific  ( floatingOrInteger )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-plus --------------------------

import TastyPlus  ( propInvertibleText, runTestsP, runTestsReplay, runTestTree )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( Arbitrary( arbitrary ), Gen
                              , oneof, testProperty )

-- template-haskell --------------------

import Language.Haskell.TH         ( Exp( AppE, ConE, LitE, TupE, VarE )
                                   , Lit( IntegerL ) )
import Language.Haskell.TH.Syntax  ( Lift( lift ) )

-- text --------------------------------

import Data.Text  ( Text, pack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- time --------------------------------

import Data.Time  ( Day, addDays, fromGregorian, toGregorian )

-- yaml --------------------------------

import Data.Yaml  ( FromJSON( parseJSON ), ToJSON( toJSON )
                  , Value( Number, String ) )

-- yaml-plus ---------------------------

import YamlPlus         ( unYaml )
import YamlPlus.Error   ( YamlParseError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import DateImprecise.DayBounds  ( DayBounds( endDay, startDay ) )
import DateImprecise.DayOfM     ( DayOfM, dayOfM )
import DateImprecise.Month      ( Month, month )
import DateImprecise.Year       ( Year, year )

--------------------------------------------------------------------------------

{- | A date, with variable precision -}
data DateImprecise = DateDay Day | DateMonth (Year,Month) | DateYear Year
  deriving (Eq,Show)

instance Lift DateImprecise where
  lift (DateDay d) = let litI ∷ Integral α ⇒ α → Exp
                         litI = LitE ∘ IntegerL ∘ toInteger

                         (y,m,dom) = toGregorian d
                      in return $ AppE (ConE 'DateDay)
                                       (AppE (AppE (AppE (VarE 'fromGregorian)
                                                         (litI y))
                                                   (litI m))
                                             (litI dom))
  lift (DateMonth (y,m)) = do y' ← lift y
                              m' ← lift m
                              return $ AppE (ConE 'DateMonth) (TupE [ y', m' ])
  lift (DateYear y) = do y' ← lift y
                         return $ AppE (ConE 'DateYear) y'

----------------------------------------

{- | Convert a day to a DateImprecise, with "clipping" as defined in
     `fromGregorian`; out-of-range days will be "clipped" back to the earliest
     available day in the month -}
dateDay_ ∷ Year → Month → DayOfM → DateImprecise
dateDay_ y m dom =
  DateDay $ fromGregorian (toNum y) (toNum m) (toNum dom)

----------

dateMonth ∷ Year → Month → DateImprecise
dateMonth = curry DateMonth

----------

dateYear ∷ Year → DateImprecise
dateYear = DateYear

----------------------------------------

cdayToDate ∷ Day → (Year,Month,DayOfM)
cdayToDate cday = let (y,m,d) = toGregorian cday
                   in (__fromI y, __fromI m, __fromI d)

----------------------------------------

textDate ∷ (Year,Month,DayOfM) → Text
textDate (y,m,d) = [fmt|%4d-%02d-%02d|] (toNumW16 y) (toNumW8 m) (toNumW8 d)

----------------------------------------

textDay ∷ Day → Text
textDay cday = let (y,m,d) = cdayToDate cday
                in textDate (y,m,d)

----------------------------------------

instance Arbitrary DateImprecise where
  arbitrary ∷ Gen DateImprecise
  arbitrary = oneof [ dateDay_  ⊳ arbitrary ⊵ arbitrary ⊵ arbitrary
                    , dateMonth ⊳ arbitrary ⊵ arbitrary
                    , dateYear  ⊳ arbitrary
                    ]

----------------------------------------

instance Printable DateImprecise where
  print (DateDay cday)    = P.text $ textDay cday
  print (DateMonth (y,m)) = P.text $ [fmt|%4d-%02d|] (toNumW16 y) (toNumW8 m)
  print (DateYear y)      = P.text $ [fmt|%4d|] (toNumW16 y)

--------------------

printableTests ∷ TestTree
printableTests =
  testGroup "Printable"
            [ testCase "2019-11-14" $ "2019-11-14" ≟ toText testDateImprecise
            , testCase "2019-11"    $ "2019-11"    ≟ toText testDateImpreciseM
            , testCase "2019"       $ "2019"       ≟ toText testDateImpreciseY
            ]

----------------------------------------

instance Textual DateImprecise where
  textual =
    tries $   (dateDay_ ⊳ textual ⋪ string "-" ⊵ textual ⋪ string "-" ⊵ textual)
          :| [ dateMonth ⊳ textual ⋪ string "-" ⊵ textual
             , DateYear  ⊳ textual ]

--------------------

textualTests ∷ TestTree
textualTests =
  let check s d = testCase s $ Just d ≟ fromString s
   in testGroup "Textual"
            [ check "2019-11-14" testDateImprecise
            , check "2019-11" testDateImpreciseM
            , check "2019" testDateImpreciseY
            , testProperty "invertibleText" (propInvertibleText @DateImprecise)
            ]

----------------------------------------

instance Parsecable DateImprecise where
  parser = textual

--------------------

parsecableTests ∷ TestTree
parsecableTests =
  let nn = "" ∷ String
      testDateImpreciseString  = "2019-11-14" ∷ String
      testDateImpreciseMString = "2019-11" ∷ String
      testDateImpreciseYString = "2019" ∷ String
      check s d = testCase s $ Right d ≟ parsec' @DateImprecise nn s
   in testGroup "Parsecable"
                [ check testDateImpreciseString  testDateImprecise
                , check testDateImpreciseMString testDateImpreciseM
                , check testDateImpreciseYString testDateImpreciseY
                ]

----------------------------------------

instance FromJSON DateImprecise where
  parseJSON (String t) = case parseText t of
                           Parsed      d → return d
                           Malformed _ e → fail $ [fmt|%s (%t)|] e  t
  parseJSON (Number n) = case floatingOrInteger @Float @Integer n of
                             Left  f → fail $ [fmt|fractional year: (%f)|] f
                             Right i → case fromI i of
                                         Just  y → return $ dateYear y
                                         Nothing → fail $ [fmt|bad year: %d|] i
  parseJSON invalid    = typeMismatch "DateImprecise" invalid

--------------------

fromJSONTests ∷ TestTree
fromJSONTests =
  testGroup "FromJSON"
            [ testCase "2019-11-14" $
                Right testDateImprecise  ≟ unYaml @YamlParseError "2019-11-14"
            , testCase "2019-11" $
                Right testDateImpreciseM ≟ unYaml @YamlParseError "2019-11"
            , testCase "2019" $
                Right testDateImpreciseY ≟ unYaml @YamlParseError "2019"
            ]

----------------------------------------

instance ToJSON DateImprecise where
  toJSON d = String $ toText d

--------------------

toJSONTests ∷ TestTree
toJSONTests =
  let check s d = testCase s $ String (pack s) ≟ toJSON d
   in testGroup "ToJSON" [ check "2019-11-14" testDateImprecise
                         , check "2019-11"    testDateImpreciseM
                         , check "2019"       testDateImpreciseY
                         ]

----------------------------------------

endDateOfMonth ∷ Year → Month → Day
endDateOfMonth y m =
  addDays (-1) $ addDays 1 $ fromGregorian (toNum y) (toNum m) 31

{- | Convert a `DateImprecise` to a Day (the first available day in that
     "range"). -}
instance DayBounds DateImprecise where
  startDay ∷ DateImprecise → Day
  startDay (DateDay d)       = d
  startDay (DateMonth (y,m)) = fromGregorian (toNum y) (toNum m) 1
  startDay (DateYear y)      = fromGregorian (toNum y) 1 1

  endDay ∷ DateImprecise → Day
  endDay (DateDay d)       = d
  endDay (DateMonth (y,m)) = endDateOfMonth y m
  endDay (DateYear y)      = fromGregorian (toNum y) 12 31

dayBoundsTests ∷ TestTree
dayBoundsTests =
  testGroup "DayBounds"
            [ testCase "startDay 2019-11-14" $
                  fromGregorian 2019 11 14 ≟ startDay testDateImprecise
            , testCase "endDay 2019-11-14" $
                  fromGregorian 2019 11 14 ≟ endDay testDateImprecise
            , testCase "startDay 2019-11" $
                  fromGregorian 2019 11 01 ≟ startDay testDateImpreciseM
            , testCase "endDay 2019-11" $
                  fromGregorian 2019 11 30 ≟ endDay testDateImpreciseM
            , testCase "startDay 2019" $
                  fromGregorian 2019 01 01 ≟ startDay testDateImpreciseY
            , testCase "endDay 2019" $
                  fromGregorian 2019 12 31 ≟ endDay testDateImpreciseY
            ]

-- testing ---------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "DateImpreciseType" [ {- dateImpreciseRangeTests
                                        , dateImpreciseRangePrintableTests
                                        , dateImpreciseRangeTextualTests
                                        , dateImpreciseRangeParsecableTests
                                        , dateImpreciseRangeFromJSONTests
                                        , dateImpreciseRangeToJSONTests -}
                                        printableTests, textualTests
                                      , parsecableTests
                                      , fromJSONTests, toJSONTests
                                      , dayBoundsTests
                                      ]

------------------------------------------------------------
--                       test data                        --
------------------------------------------------------------

testDateImprecise ∷ DateImprecise
testDateImprecise = dateDay_ [year|2019|] [month|11|] [dayOfM|14|]

testDateImpreciseM ∷ DateImprecise
testDateImpreciseM = dateMonth [year|2019|] [month|11|]

testDateImpreciseY ∷ DateImprecise
testDateImpreciseY = dateYear [year|2019|]

------------------------------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
