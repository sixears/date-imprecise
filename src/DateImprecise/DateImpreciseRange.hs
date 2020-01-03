{-# LANGUAGE DeriveLift          #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnicodeSyntax       #-}

{- | Date range, utilizing DateImprecise for varying precisions -}

module DateImprecise.DateImpreciseRange
  ( DateImpreciseRange, dateImpreciseRange, dateImpreciseR, dateImpreciseR'
  , tests )
where

import Prelude  ( Int )

-- aeson -------------------------------

import Data.Aeson.Types  ( Value( Number, String ), typeMismatch )

-- base --------------------------------

import Control.Monad       ( Monad, fail, return )
import Data.Bifunctor      ( bimap )
import Data.Bool           ( Bool( False, True ), otherwise )
import Data.Either         ( Either( Left, Right ) )
import Data.Eq             ( Eq( (==) ) )
import Data.Function       ( ($), (&) )
import Data.List.NonEmpty  ( NonEmpty( (:|) ) )
import Data.Maybe          ( Maybe( Just, Nothing ) )
import Data.Ord            ( (<), max, min )
import Data.String         ( String )
import Data.Tuple          ( uncurry )
import System.Exit         ( ExitCode )
import System.IO           ( IO )
import Text.Show           ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode      ( (∧) )
import Data.Eq.Unicode        ( (≡), (≢) )
import Data.Function.Unicode  ( (∘) )
import Data.Ord.Unicode       ( (≤), (≥) )

-- data-default ------------------------

import Data.Default  ( def )

-- data-textual ------------------------

import Data.Textual  ( Parsed( Parsed, Malformed ), Printable( print )
                     , Textual( textual )
                     , parseText, fromString, toString, toText
                     )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵), (⋪) )
import Data.MoreUnicode.Functor      ( (⊳), (⩺) )
import Data.MoreUnicode.Lens         ( (⊩) )
import Data.MoreUnicode.Natural      ( ℕ )
import Data.MoreUnicode.Tasty        ( (≟), (≣) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- parsec-plus -------------------------

import ParsecPlus  ( Parsecable( parser ), parsec' )

-- parser-plus -------------------------

import ParserPlus  ( tries )

-- parsers ------------------------------

import Text.Parser.Char  ( CharParsing, string )

-- quasiquoting ------------------------

import QuasiQuoting  ( exp, mkQQ )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary ) )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-plus --------------------------

import TastyPlus  ( propInvertibleText, runTestsP, runTestsReplay, runTestTree )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( Gen, suchThat, testProperty )

-- template-haskell --------------------

import Language.Haskell.TH.Quote   ( QuasiQuoter )
import Language.Haskell.TH.Syntax  ( Lift )

-- text --------------------------------

import Data.Text  ( Text, pack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- time --------------------------------

import Data.Time  ( Day, fromGregorian )

-- yaml --------------------------------

import Data.Yaml  ( FromJSON( parseJSON ), ToJSON( toJSON ) )

-- yaml-plus ---------------------------

import YamlPlus        ( unYaml' )
import YamlPlus.Error  ( YamlParseError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import DateImprecise.DateImprecise  ( DateImprecise
                                    , pattern DayDate , pattern MonthDate
                                    , pattern YearDate
                                    , dateDayM, dateDay_, dateMonth
                                    , dateImprecise, dateYear, dayDate
                                    , endDateOfMonth, toGregory
                                    )
import DateImprecise.Error          ( AsDateError_, DateErrorImprecise
                                    , dateRangeError, dateRangeError_ )
import DateImprecise.DayBounds      ( DayBounds( endDay, startDay ) )
import DateImprecise.DayOfM         ( DayOfM( D ), dayOfM )
import DateImprecise.Month          ( Month( M ), month )
import DateImprecise.Year           ( Year, year )

--------------------------------------------------------------------------------

{- | A date, with variable precision -}
newtype DateImpreciseRange = DateImpreciseRange (DateImprecise,DateImprecise)
  deriving (Lift,Show)

instance Eq DateImpreciseRange where
  d == d' = startDay d ≡ startDay d' ∧ endDay   d ≡ endDay   d'

dateImpreciseR ∷ (AsDateError_ DateImprecise ε, MonadError ε η) ⇒
                 DateImprecise → DateImprecise → η DateImpreciseRange
dateImpreciseR start end = if startDay start ≤ startDay end
                           then return $ DateImpreciseRange (start,end)
                           else dateRangeError start end

dateImpreciseR' ∷ MonadError DateErrorImprecise η ⇒
                  DateImprecise → DateImprecise → η DateImpreciseRange
dateImpreciseR' = dateImpreciseR

{- | Create a `DateImpreciseRange` from two dates; else call `Monad.fail`. -}
dateImpreciseRM ∷ Monad η ⇒ DateImprecise → DateImprecise → η DateImpreciseRange
dateImpreciseRM start end =
  case dateImpreciseR @DateErrorImprecise start end of
    Left  e → fail $ show e
    Right r → return r

dateImpreciseRangeTests ∷ TestTree
dateImpreciseRangeTests =
  testGroup "dateImpreciseRange"
            [ testCase "2019-11-14:2019-11-26" $
                  Right testDateImpreciseRange
                ≟ dateImpreciseR @DateErrorImprecise testDateP0 testDateP1
            , testCase "2019-11-26:2019-11-14" $
                  Left (dateRangeError_ testDateP1 testDateP0)
                ≟ dateImpreciseR @DateErrorImprecise testDateP1 testDateP0
            , testCase "2019-11-14:2019-11-14" $
                  Right (DateImpreciseRange (testDateP0, testDateP0))
                ≟ dateImpreciseR @DateErrorImprecise testDateP0 testDateP0
            , testCase "2019:2019" $
                  Right (DateImpreciseRange (testDatePY1, testDatePY1))
                ≟ dateImpreciseR @DateErrorImprecise testDatePY1 testDatePY1
            , testCase "2019:2017" $
                  Left (dateRangeError_ testDatePY1 testDatePY0)
                ≟ dateImpreciseR @DateErrorImprecise testDatePY1 testDatePY0
            ]

----------------------------------------

instance Printable DateImpreciseRange where
  print (DateImpreciseRange ((DayDate y0 m0 d0),(DayDate y1 m1 d1)))
            | y0 ≡ y1 ∧ m0 ≡ m1 ∧ d0 ≡ d1 = P.text $ [fmt|%T-%T-%T|] y0 m0 d0
            | y0 ≡ y1 ∧ m0 ≡ m1  = P.text $ [fmt|%T-%T-%T:%T|]    y0 m0 d0 d1
            | y0 ≡ y1            = P.text $ [fmt|%T-%T-%T:%T-%T|] y0 m0 d0 m1 d1
  print (DateImpreciseRange ((MonthDate y0 m0),(MonthDate y1 m1)))
            | y0 ≡ y1 ∧ m0 ≡ m1  = P.text $ [fmt|%T-%T|]       y0 m0
            | y0 ≡ y1            = P.text $ [fmt|%T-%T:%T|]    y0 m0 m1
  print (DateImpreciseRange ((MonthDate y0 m0),(DayDate y1 m1 d1)))
            | y0 ≡ y1            = P.text $ [fmt|%T-%T:%T-%T|] y0 m0 m1 d1
  print (DateImpreciseRange ((YearDate y0),(DayDate y1 m1 d1)))
--          we don't do this, because the output (e.g., 2019-05:07 is
--          indistinguishable from MonthDate-MonthDate where the years are equal
--            | y0 ≡ y1 ∧ m0 ≡ m1  = P.text $ [fmt|%T-%T:%T|] y0 m0 d1
            | y0 ≡ y1            = P.text $ [fmt|%T:%T-%T|] y0 m1 d1
  print (DateImpreciseRange ((YearDate y0),(MonthDate y1 m1)))
--          we can safely re-parse this, it is distinct from year:year, because
--          of the number of digits after the colon
            | y0 ≡ y1            = P.text $ [fmt|%T:%T|] y0 m1
  print (DateImpreciseRange ((YearDate y0),(YearDate y1)))
            | y0 ≡ y1           = P.text $ [fmt|%T|] y0
  print (DateImpreciseRange (d0,d1))  = P.text $ [fmt|%T:%T|] d0 d1

--------------------

dateImpreciseRangePrintableTests ∷ TestTree
dateImpreciseRangePrintableTests =
  let check s dpr = testCase s $ s ≟ toString dpr
   in testGroup "Printable"
                [ check "2019-11-14:26"         testDateImpreciseRange
                , check "2017-11-26:2019-11-26" testDateImpreciseRange1
                , check "2017-11-26"            testDateImpreciseRange2
                , check "2017-11:2019-11"       testDateImpreciseRangeM
                , check "2019-05:11"            testDateImpreciseRangeM1
                , check "2019-05"               testDateImpreciseRangeM2
                , check "2017:2019"             testDateImpreciseRangeY
                , check "2019"                  testDateImpreciseRangeY2
                , check "2019-11:11-14"         testDateImpreciseRangeMD0
                , check "2019-05:11-26"         testDateImpreciseRangeMD1
                , check "2019:11-14"            testDateImpreciseRangeYD1
                , check "2019:05"               testDateImpreciseRangeYM
                ]

----------------------------------------

instance Parsecable DateImpreciseRange where
  parser = textual

--------------------

dateImpreciseRangeParsecableTests ∷ TestTree
dateImpreciseRangeParsecableTests =
  let check s dpr = testCase s $ Right dpr ≟ parsec' (""∷Text) s
   in testGroup "Parsecable"
                [ check "2019-11-14:2019-11-26" testDateImpreciseRange
                , check "2019-11-14:26"         testDateImpreciseRange
                , check "2019-11-14:11-26"      testDateImpreciseRange
                , check "2017-11-26:2019-11-26" testDateImpreciseRange1
                , check "2017-11-26"            testDateImpreciseRange2
                , check "2017-11-26:2017-11-26" testDateImpreciseRange2
                , check "2017-11:2019-11"       testDateImpreciseRangeM
                , check "2019-05:2019-11"       testDateImpreciseRangeM1
                , check "2019-05:11"            testDateImpreciseRangeM1
                , check "2019-05"               testDateImpreciseRangeM2
                , check "2019-05:2019-05"       testDateImpreciseRangeM2
                , check "2017:2019"             testDateImpreciseRangeY
                , check "2019:2019"             testDateImpreciseRangeY2
                , check "2019"                  testDateImpreciseRangeY2
                , check "2019-11:11-14"         testDateImpreciseRangeMD0
                , check "2019-05:11-26"         testDateImpreciseRangeMD1
                , check "2019:11-14"            testDateImpreciseRangeYD1
                , check "2019:05"               testDateImpreciseRangeYM
                ]

----------------------------------------

dateImpreciseMM ∷ Monad η ⇒ (DateImprecise,DateImprecise) → η DateImpreciseRange
dateImpreciseMM = uncurry dateImpreciseRM

----------

parseDD ∷ (Monad η, CharParsing η) ⇒ η DateImpreciseRange
parseDD = do
  (d,d') ← (,) ⊳ textual ⋪ string ":" ⊵ textual
  dateImpreciseMM (d,d')

----------

parseD ∷ (Monad η, CharParsing η) ⇒ η DateImpreciseRange
parseD = do
  d ← textual
  dateImpreciseMM (d,d)

--------------------

dateImpreciseYMDD ∷ Monad η ⇒ (Year,Month,DayOfM,DayOfM) → η DateImpreciseRange
dateImpreciseYMDD (y,m,d0,d1) = do
  start ← dateDayM y m d0
  end   ← dateDayM y m d1
  case dateImpreciseR @DateErrorImprecise start end of
    Left  e → fail $ show e
    Right r → return r

----------

parseYMDD ∷ (Monad η, CharParsing η) ⇒ η DateImpreciseRange
parseYMDD = do
  (y,m,d0,d1) ← (,,,) ⊳ textual ⋪ string "-" ⊵ textual
                      ⋪ string "-" ⊵ textual ⋪ string ":" ⊵ textual
  dateImpreciseYMDD (y,m,d0,d1)

--------------------

dateImpreciseYMMDD ∷ Monad η ⇒
                     (Year,Month,DayOfM,Month,DayOfM) → η DateImpreciseRange
dateImpreciseYMMDD (y,m0,d0,m1,d1) = do
  start ← dateDayM y m0 d0
  end   ← dateDayM y m1 d1
  case dateImpreciseR @DateErrorImprecise start end of
    Left  e → fail $ show e
    Right r → return r

----------

parseYMMDD ∷ (Monad η, CharParsing η) ⇒ η DateImpreciseRange
parseYMMDD = do
  (y,m0,d0,m1,d1) ← (,,,,) ⊳ textual ⋪ string "-" ⊵ textual ⋪ string "-"
                           ⊵ textual ⋪ string ":"
                           ⊵ textual ⋪ string "-" ⊵ textual
  dateImpreciseYMMDD (y,m0,d0,m1,d1)

--------------------

dateImpreciseYMM ∷ Monad η ⇒ (Year,Month,Month) → η DateImpreciseRange
dateImpreciseYMM (y,m0,m1) = do
  let start = dateMonth y m0
  let end   = dateMonth y m1
  case dateImpreciseR @DateErrorImprecise start end of
    Left  e → fail $ show e
    Right r → return r

----------

parseYMM ∷ (Monad η, CharParsing η) ⇒ η DateImpreciseRange
parseYMM = do
  (y,m0,m1) ← (,,) ⊳ textual ⋪ string "-" ⊵ textual ⋪ string ":" ⊵ textual
  dateImpreciseYMM (y,m0,m1)

--------------------

dateImpreciseYMMD ∷ Monad η ⇒ (Year,Month,Month,DayOfM) → η DateImpreciseRange
dateImpreciseYMMD (y,m0,m1,d1) = do
  let start = dateMonth y m0
  end ← dateDayM y m1 d1
  case dateImpreciseR @DateErrorImprecise start end of
    Left  e → fail $ show e
    Right r → return r

----------

parseYMMD ∷ (Monad η, CharParsing η) ⇒ η DateImpreciseRange
parseYMMD = do
  (y,m0,m1,d1) ← (,,,) ⊳ textual ⋪ string "-" ⊵ textual
                       ⋪ string ":"
                       ⊵ textual ⋪ string "-" ⊵ textual
  dateImpreciseYMMD (y,m0,m1,d1)

--------------------

dateImpreciseYMD ∷ Monad η ⇒ (Year,Month,DayOfM) → η DateImpreciseRange
dateImpreciseYMD (y,m1,d1) = do
  let start = dateYear y
  end ← dateDayM y m1 d1
  case dateImpreciseR @DateErrorImprecise start end of
    Left  e → fail $ show e
    Right r → return r

----------

{- | Parse year, followed by month then day of month. -}
parseYMD ∷ (Monad η, CharParsing η) ⇒ η DateImpreciseRange
parseYMD = do
  (y,m1,d1) ← (,,) ⊳ textual ⋪ string ":" ⊵ textual ⋪ string "-" ⊵ textual
  dateImpreciseYMD (y,m1,d1)

--------------------

dateImpreciseYM ∷ Monad η ⇒ (Year,Month) → η DateImpreciseRange
dateImpreciseYM (y,m) = do
  let start = dateYear y
      end   = dateMonth y m
  case dateImpreciseR @DateErrorImprecise start end of
    Left  e → fail $ show e
    Right r → return r

----------

parseYM ∷ (Monad η, CharParsing η) ⇒ η DateImpreciseRange
parseYM = do
  (y,m) ← (,) ⊳ textual ⋪ string ":" ⊵ textual
  dateImpreciseYM (y,m)

--------------------

instance Textual DateImpreciseRange where
  textual = tries $ parseYMMDD :| [ parseYMDD, parseDD, parseYMMD, parseYMM
                                  , parseYMD, parseYM, parseD ]

--------------------

dateImpreciseRangeTextualTests ∷ TestTree
dateImpreciseRangeTextualTests =
  let check s d = testCase s $ Just d ≟ fromString s
   in testGroup "Textual"
                [ check "2019-11-14:2019-11-26" testDateImpreciseRange
                , check "2019-11-14:26"         testDateImpreciseRange
                , check "2019-11-14:11-26"      testDateImpreciseRange
                , check "2017-11-26:2019-11-26" testDateImpreciseRange1
                , check "2017-11-26"            testDateImpreciseRange2
                , check "2017-11-26:2017-11-26" testDateImpreciseRange2
                , check "2017-11:2019-11"       testDateImpreciseRangeM
                , check "2019-05:2019-11"       testDateImpreciseRangeM1
                , check "2019-05:11"            testDateImpreciseRangeM1
                , check "2019-05"               testDateImpreciseRangeM2
                , check "2019-05:2019-05"       testDateImpreciseRangeM2
                , check "2017:2019"             testDateImpreciseRangeY
                , check "2019:2019"             testDateImpreciseRangeY2
                , check "2019"                  testDateImpreciseRangeY2
                , check "2019-11:11-14"         testDateImpreciseRangeMD0
                , check "2019-05:11-26"         testDateImpreciseRangeMD1
                , check "2019:11-14"            testDateImpreciseRangeYD1
                , check "2019:12-24"            testDateImpreciseRangeYD2
                , check "2019:05"               testDateImpreciseRangeYM
                , testProperty "invertibleText"
                    (propInvertibleText @DateImprecise)
                ]

----------------------------------------

instance Arbitrary DateImpreciseRange where
  arbitrary ∷ Gen DateImpreciseRange
  arbitrary = let isValid ∷ DateImpreciseRange → Bool
                  isValid (DateImpreciseRange (d0,d1)) =
                    case dateImpreciseR @DateErrorImprecise d0 d1 of
                      Left  _ → False
                      Right _ → True
               in (DateImpreciseRange ⊳ ((,) ⊳ arbitrary ⊵ arbitrary))
                  `suchThat` isValid

----------------------------------------

instance FromJSON DateImpreciseRange where
  parseJSON (String t) = case parseText t of
                           Parsed      d → return d
                           Malformed _ e → fail $ [fmt|%s (%t)|] e  t
  parseJSON y@(Number _) = do y' <- parseJSON y
                              return $ DateImpreciseRange (y',y')
  parseJSON invalid    = typeMismatch "DateImprecise" invalid

--------------------

dateImpreciseRangeFromJSONTests ∷ TestTree
dateImpreciseRangeFromJSONTests =
  let check s d = testCase s $ Right d ≟ unYaml' @YamlParseError s
   in testGroup "FromJSON"
                [ check "2019-11-14:2019-11-26" testDateImpreciseRange
                , check "2019-11-14:26"         testDateImpreciseRange
                , check "2019-11-14:11-26"      testDateImpreciseRange
                , check "2017-11-26:2019-11-26" testDateImpreciseRange1
                , check "2017-11-26"            testDateImpreciseRange2
                , check "2017-11-26:2017-11-26" testDateImpreciseRange2
                , check "2017-11:2019-11"       testDateImpreciseRangeM
                , check "2019-05:2019-11"       testDateImpreciseRangeM1
                , check "2019-05:11"            testDateImpreciseRangeM1
                , check "2019-05"               testDateImpreciseRangeM2
                , check "2019-05:2019-05"       testDateImpreciseRangeM2
                , check "2017:2019"             testDateImpreciseRangeY
                , check "2019:2019"             testDateImpreciseRangeY2
                , check "2019"                  testDateImpreciseRangeY2
                , check "2019-11:11-14"         testDateImpreciseRangeMD0
                , check "2019-05:11-26"         testDateImpreciseRangeMD1
                , check "2019:11-14"            testDateImpreciseRangeYD1
                , check "2019:12-24"            testDateImpreciseRangeYD2
                , check "2019:05"               testDateImpreciseRangeYM
                ]

----------------------------------------

instance ToJSON DateImpreciseRange where
  toJSON d = String $ toText d

--------------------

dateImpreciseRangeToJSONTests ∷ TestTree
dateImpreciseRangeToJSONTests =
  let check s d = testCase s $ String (pack s) ≟ toJSON d
   in testGroup "ToJSON"
                [ check "2019-11-14:26"         testDateImpreciseRange
                , check "2017-11-26:2019-11-26" testDateImpreciseRange1
                , check "2017-11-26"            testDateImpreciseRange2
                , check "2017-11:2019-11"       testDateImpreciseRangeM
                , check "2019-05:11"            testDateImpreciseRangeM1
                , check "2019-05"               testDateImpreciseRangeM2
                , check "2017:2019"             testDateImpreciseRangeY
                , check "2019"                  testDateImpreciseRangeY2
                , check "2019-11:11-14"         testDateImpreciseRangeMD0
                , check "2019-05:11-26"         testDateImpreciseRangeMD1
                , check "2019:11-14"            testDateImpreciseRangeYD1
                , check "2019:12-24"            testDateImpreciseRangeYD2
                , check "2019:05"               testDateImpreciseRangeYM
                ]

----------------------------------------

dateImpreciseRange ∷ QuasiQuoter
dateImpreciseRange =
  mkQQ "DateImpreciseRange" $
    def & exp ⊩ ((\ r → ⟦r⟧) ⩺ fromString @DateImpreciseRange)

----------------------------------------

{- | The first day of a range. -}
instance DayBounds DateImpreciseRange where
  startDay ∷ DateImpreciseRange → Day
  startDay (DateImpreciseRange (d0,_)) = startDay d0
  endDay ∷ DateImpreciseRange → Day
  endDay (DateImpreciseRange (_,d1)) = endDay d1


dayBoundsTests ∷ TestTree
dayBoundsTests =
  let checkS y m o d = testCase "startDay" $ fromGregorian y m o ≟ startDay d
      checkE y m o d = testCase "endDay"   $ fromGregorian y m o ≟ endDay d
      check  y0 m0 d0 y1 m1 d1 d = testGroup (toString d) $ [ checkS y0 m0 d0 d
                                                            , checkE y1 m1 d1 d ]
   in testGroup "DayBounds"
                [ check 2019 11 14 2019 11 26 testDateImpreciseRange
                , check 2017 11 26 2019 11 26 testDateImpreciseRange1
                , check 2017 11 26 2017 11 26 testDateImpreciseRange2
                , check 2017 11 01 2019 11 30 testDateImpreciseRangeM
                , check 2019 05 01 2019 11 30 testDateImpreciseRangeM1
                , check 2019 05 01 2019 05 31 testDateImpreciseRangeM2
                , check 2019 11 01 2019 11 14 testDateImpreciseRangeMD0
                , check 2019 05 01 2019 11 26 testDateImpreciseRangeMD1
                , check 2017 01 01 2019 12 31 testDateImpreciseRangeY
                , check 2019 01 01 2019 12 31 testDateImpreciseRangeY2
                , check 2019 01 01 2019 11 14 testDateImpreciseRangeYD1
                , check 2019 01 01 2019 12 24 testDateImpreciseRangeYD2
                , check 2019 01 01 2019 05 31 testDateImpreciseRangeYM
                ]

----------------------------------------

{- | Convert to the most general representation available;
     e.g., 2019-01-01:2019-01-31 generalizes to 2019-01. -}
generalize ∷ DateImpreciseRange → DateImpreciseRange
generalize d =
  let sd = startDay d
      ed = endDay   d
      dirYs y0 y1       = DateImpreciseRange (dateYear y0,dateYear y1)
      dirYM y0 y1 m1    = DateImpreciseRange (dateYear y0,dateMonth y1 m1)
      dirMs y0 m0 y1 m1 = DateImpreciseRange (dateMonth y0 m0,dateMonth y1 m1)
      dirYD y0 d1       = DateImpreciseRange (dateYear y0,dayDate d1)
      dirMD y0 m0 d1    = DateImpreciseRange (dateMonth y0 m0,dayDate d1)
      dirDM d0 y1 m1    = DateImpreciseRange (dayDate d0,dateMonth y1 m1)
      dirDs d0 d1       = DateImpreciseRange (dayDate d0,dayDate d1)
   in case bimap toGregory toGregory (startDay d, endDay d) of
        ((sy,M (1∷Int),D (1∷Int)),(ey,M (12∷Int),D (31∷Int))) → dirYs sy ey
        ((sy,M (1∷Int),D (1∷Int)),(ey,em,_))
                          | ed ≡ endDateOfMonth ey em → dirYM sy ey em
                          | otherwise                 → dirYD sy ed
        ((sy,sm,D (1∷Int)),(ey,em,_))
                          | ed ≡ endDateOfMonth ey em → dirMs sy sm ey em
                          | otherwise                 → dirMD sy sm ed
        ((_,sm,_),(ey,em,_))
                          | sm ≢ em ∧ ed ≡ endDateOfMonth ey em → dirDM sd ey em
                          | otherwise                           → dirDs sd ed

generalizeTests ∷ TestTree
generalizeTests =
  let check r0 r1 d0 d1 = let d = DateImpreciseRange (d0,d1)
                              DateImpreciseRange (g0,g1) = generalize d
                           in testCase ([fmt|%T:%T -> %T:%T (%T:%T)|]
                                             d0 d1    r0 r1  g0 g1) $
                                  (r0,r1) ≟ (g0,g1)
   in testGroup "generalize"
                [ check [dateImprecise|2019|]       [dateImprecise|2019-01|]
                        [dateImprecise|2019-01-01|] [dateImprecise|2019-01-31|]
                , check [dateImprecise|2019-01-02|] [dateImprecise|2019-01-31|]
                        [dateImprecise|2019-01-02|] [dateImprecise|2019-01-31|]
                , check [dateImprecise|2019-02|] [dateImprecise|2019-03|]
                        [dateImprecise|2019-02-01|] [dateImprecise|2019-03-31|]
                , check [dateImprecise|2019-02-02|] [dateImprecise|2019-03|]
                        [dateImprecise|2019-02-02|] [dateImprecise|2019-03-31|]
                , check [dateImprecise|2019-02|]    [dateImprecise|2019-03-30|]
                        [dateImprecise|2019-02-01|] [dateImprecise|2019-03-30|]
                , check [dateImprecise|2019-02-02|] [dateImprecise|2019-03-30|]
                        [dateImprecise|2019-02-02|] [dateImprecise|2019-03-30|]
                , check [dateImprecise|2019-02-02|] [dateImprecise|2019-02-28|]
                        [dateImprecise|2019-02-02|] [dateImprecise|2019-02-28|]
                , check [dateImprecise|2019-02|]    [dateImprecise|2019-02|]
                        [dateImprecise|2019-02-01|] [dateImprecise|2019-02-28|]
                , check [dateImprecise|2019|]       [dateImprecise|2019-05|]
                        [dateImprecise|2019-01-01|] [dateImprecise|2019-05-31|]
                , check [dateImprecise|2019|]       [dateImprecise|2019|]
                        [dateImprecise|2019-01-01|] [dateImprecise|2019-12-31|]
                , check [dateImprecise|2017|]       [dateImprecise|2019|]
                        [dateImprecise|2017-01-01|] [dateImprecise|2019-12-31|]

                , check [dateImprecise|2019|]       [dateImprecise|2019-12-30|]
                        [dateImprecise|2019-01-01|] [dateImprecise|2019-12-30|]
                , check [dateImprecise|2019-01-02|] [dateImprecise|2019-12-30|]
                        [dateImprecise|2019-01-02|] [dateImprecise|2019-12-30|]
                , check [dateImprecise|2019-04|]    [dateImprecise|2019-05|]
                        [dateImprecise|2019-04-01|] [dateImprecise|2019-05|]
                , testProperty "maintainBounds" (\ r → let g = generalize r
                                                           gs = startDay g
                                                           ge = endDay   g
                                                           rs = startDay r
                                                           re = endDay   r
                                                        in (gs,ge) ≣ (rs,re)
                                                )
                ]

{- | Convert to the most specific representation available; which in practice is      always as a range of days e.g., 2019-01 specifizes to
     2019-01-01:2019-01-31.
 -}
specifize ∷ DateImpreciseRange → DateImpreciseRange
specifize a = DateImpreciseRange (dayDate $ startDay a, dayDate $ endDay a)

specifizeTests ∷ TestTree
specifizeTests =
  testGroup "specifize"
            [ testProperty "maintainBounds" (\ r → let s = specifize r
                                                       ss = startDay s
                                                       se = endDay   s
                                                       rs = startDay r
                                                       re = endDay   r
                                                    in (ss,se) ≣ (rs,re))
            ]

{- | If two ranges overlap or are precisely consecutive (i.e., one finishes on
     date x, the other starts on date x+1), then merge them into one range.
     That range will be as general as possible (see `generalize`).
 -}
merge ∷ DateImpreciseRange → DateImpreciseRange → Maybe DateImpreciseRange
merge a b = if startDay b < startDay a
            then merge b a
            else -- startDay a ≤ startDay b
                 let e = dayDate $ max (endDay a) (endDay b)
                 in if startDay a ≡ startDay b
                    then Just ∘ generalize $
                           DateImpreciseRange (dayDate $ startDay a, e)
                    else -- startDay a < startDay b
                         if endDay a ≥ startDay b
                         then Just ∘ generalize $
                                DateImpreciseRange (dayDate $ startDay a, e)
                         else -- endDay a < startDay b
                              Nothing

mergeTests ∷ TestTree
mergeTests =
  let check r0 r1 d0 d1 e0 e1 = let d = DateImpreciseRange (d0,d1)
                                    e = DateImpreciseRange (e0,e1)
                                 in testCase (toString d) $
                                        Just (DateImpreciseRange (r0,r1))
                                      ≟ merge d e
      checkNon d0 d1 e0 e1 = let d = DateImpreciseRange (d0,d1)
                                 e = DateImpreciseRange (e0,e1)
                              in testCase (toString d) $ Nothing ≟ merge d e

      mergeProp r r' = let m = merge r r'
                        in case m of
                             Just m' → startDay m' ≡ min (startDay r)
                                                         (startDay r')
                                     ∧ endDay   m' ≡ max (endDay r) (endDay r')
                             Nothing → True

   in testGroup "merge"
                [ check [dateImprecise|2019-01|]    [dateImprecise|2019-01|]
                        [dateImprecise|2019-01-01|] [dateImprecise|2019-01-10|]
                        [dateImprecise|2019-01-10|] [dateImprecise|2019-01-31|]
                , check [dateImprecise|2019-01-02|] [dateImprecise|2019-01-31|]
                        [dateImprecise|2019-01-02|] [dateImprecise|2019-01-10|]
                        [dateImprecise|2019-01-10|] [dateImprecise|2019-01-31|]
                , checkNon
                        [dateImprecise|2019-01-01|] [dateImprecise|2019-01-10|]
                        [dateImprecise|2019-01-12|] [dateImprecise|2019-01-31|]
                , checkNon
                        [dateImprecise|2019-01-12|] [dateImprecise|2019-01-31|]
                        [dateImprecise|2019-01-01|] [dateImprecise|2019-01-10|]
                , testProperty "maintainBounds" mergeProp
                ]

-- testing ---------------------------------------------------------------------

------------------------------------------------------------
--                       test data                        --
------------------------------------------------------------

testDateP0 ∷ DateImprecise
testDateP0 = dateDay_ [year|2019|] [month|11|] [dayOfM|14|]

testDateP1 ∷ DateImprecise
testDateP1 = dateDay_ [year|2019|] [month|11|] [dayOfM|26|]

testDateP2 ∷ DateImprecise
testDateP2 = dateDay_ [year|2017|] [month|11|] [dayOfM|26|]

testDateP3 ∷ DateImprecise
testDateP3 = dateDay_ [year|2019|] [month|12|] [dayOfM|24|]

testDateImpreciseRange ∷ DateImpreciseRange
testDateImpreciseRange = DateImpreciseRange (testDateP0,testDateP1)

testDateImpreciseRange1 ∷ DateImpreciseRange
testDateImpreciseRange1 = DateImpreciseRange (testDateP2,testDateP1)

testDateImpreciseRange2 ∷ DateImpreciseRange
testDateImpreciseRange2 = DateImpreciseRange (testDateP2,testDateP2)

testDatePM0 ∷ DateImprecise
testDatePM0 = dateMonth [year|2017|] [month|11|]

testDatePM1 ∷ DateImprecise
testDatePM1 = dateMonth [year|2019|] [month|11|]

testDatePM2 ∷ DateImprecise
testDatePM2 = dateMonth [year|2019|] [month|05|]

testDateImpreciseRangeM ∷ DateImpreciseRange
testDateImpreciseRangeM = DateImpreciseRange (testDatePM0,testDatePM1)

testDateImpreciseRangeM1 ∷ DateImpreciseRange
testDateImpreciseRangeM1 = DateImpreciseRange (testDatePM2,testDatePM1)

testDateImpreciseRangeM2 ∷ DateImpreciseRange
testDateImpreciseRangeM2 = DateImpreciseRange (testDatePM2,testDatePM2)

testDateImpreciseRangeMD0 ∷ DateImpreciseRange
testDateImpreciseRangeMD0 = DateImpreciseRange (testDatePM1,testDateP0)

testDateImpreciseRangeMD1 ∷ DateImpreciseRange
testDateImpreciseRangeMD1 = DateImpreciseRange (testDatePM2,testDateP1)

testDatePY0 ∷ DateImprecise
testDatePY0 = dateYear [year|2017|]

testDatePY1 ∷ DateImprecise
testDatePY1 = dateYear [year|2019|]

testDateImpreciseRangeY ∷ DateImpreciseRange
testDateImpreciseRangeY = DateImpreciseRange (testDatePY0,testDatePY1)

testDateImpreciseRangeY2 ∷ DateImpreciseRange
testDateImpreciseRangeY2 = DateImpreciseRange (testDatePY1,testDatePY1)

testDateImpreciseRangeYD1 ∷ DateImpreciseRange
testDateImpreciseRangeYD1 = DateImpreciseRange (testDatePY1,testDateP0)

testDateImpreciseRangeYD2 ∷ DateImpreciseRange
testDateImpreciseRangeYD2 = DateImpreciseRange (testDatePY1,testDateP3)

testDateImpreciseRangeYM ∷ DateImpreciseRange
testDateImpreciseRangeYM = DateImpreciseRange (testDatePY1,testDatePM2)

------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "DateImpreciseRange" [ dateImpreciseRangeTests
                                       , dateImpreciseRangePrintableTests
                                       , dateImpreciseRangeTextualTests
                                       , dateImpreciseRangeParsecableTests
                                       , dateImpreciseRangeFromJSONTests
                                       , dateImpreciseRangeToJSONTests
                                       , dayBoundsTests
                                       , generalizeTests
                                       , specifizeTests
                                       , mergeTests
                                       ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
