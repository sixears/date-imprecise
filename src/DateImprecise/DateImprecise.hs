{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

{- | Date, with varying precisions - day, month, year. -}

module DateImprecise.DateImprecise
  ( DateImprecise
  , pattern DayDate, pattern MonthDate, pattern YearDate
  , dateDay, dateDay', dateDayM, dateDay_, dateMonth, dateYear, dateImprecise
  , dayDate, endDateOfMonth, endDayOfM, toDate, toGregory

  , tests
  )
where

-- base --------------------------------

import Control.Monad  ( Monad, fail, return )
import Data.Either    ( Either( Left, Right ) )
import Data.Function  ( ($), (&) )
import Data.Maybe     ( Maybe( Just, Nothing ) )
import Data.String    ( String )
import System.Exit    ( ExitCode )
import System.IO      ( IO )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode  ( (≡) )

-- boundedn ----------------------------

import FromI  ( __fromI )
import ToNum  ( toNum )

-- data-default ------------------------

import Data.Default  ( def )

-- data-textual ------------------------

import Data.Textual  ( fromString, toString )

-- monaderror-io -----------------------

import MonadError  ( mapMError )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⩺) )
import Data.MoreUnicode.Lens     ( (⊩) )
import Data.MoreUnicode.Natural  ( ℕ )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- quasiquoting ------------------------

import QuasiQuoting  ( exp, mkQQ )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-plus --------------------------

import TastyPlus  ( runTestsP, runTestsReplay, runTestTree )

-- template-haskell --------------------

import Language.Haskell.TH.Quote  ( QuasiQuoter )

-- time --------------------------------

import Data.Time  ( Day, toGregorian )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import DateImprecise.Error              ( AsDateError_, DateErrorImprecise
                                        , badDateError, emap )
import DateImprecise.DateImpreciseType  ( DateImprecise(..), dateDay_, dateMonth
                                        , dateYear, endDateOfMonth )
import DateImprecise.DayBounds          ( DayBounds( startDay ) )
import DateImprecise.DayOfM             ( DayOfM )
import DateImprecise.Month              ( Month )
import DateImprecise.Year               ( Year )

--------------------------------------------------------------------------------

dayDate ∷ Day → DateImprecise
dayDate = DateDay

dateDay ∷ (AsDateError_ (Year,Month,DayOfM) ε, MonadError ε η) ⇒
          Year → Month → DayOfM → η DateImprecise
dateDay y m d = let (y_,m_,d_) = (toNum y, toNum m, toNum d)
                    result@(DateDay day) = dateDay_ y m d
                    (y', m', d') = toGregorian day
                 in if (y',m',d') ≡ (y_,m_,d_)
                    then return result
                    else badDateError (y,m,d)

----------

dateDay' ∷ MonadError DateErrorImprecise η ⇒
           Year → Month → DayOfM → η DateImprecise
dateDay' y m dom = let uncurry3 ∷ (α → β → γ → δ) → (α,β,γ) → δ
                       uncurry3 f (a,b,c) = f a b c
                    in mapMError (emap $ uncurry3 dateDay_) $ dateDay y m dom

----------

dateDayM ∷ Monad η ⇒ Year → Month → DayOfM → η DateImprecise
dateDayM y m d = case dateDay' y m d of
                   Left  e → fail $ toString e
                   Right r → return r

--------------------

dayMay ∷ DateImprecise → Maybe (Year,Month,DayOfM)
dayMay (DateDay d) = let (y,m,dom) = toGregorian d
                      in Just (__fromI y, __fromI m, __fromI dom)
dayMay _           = Nothing

{- | Pattern (de)constructor for a day from Y/M/D; note that out-of-bounds days;
     e.g., 31st Nov, 30th Feb, will be trimmed to the latest real prior date. -}
pattern DayDate ∷ Year → Month → DayOfM → DateImprecise
pattern DayDate y m dom ← (dayMay → Just (y,m,dom))
                          where DayDate y m dom = dateDay_ y m dom

----------------------------------------

monthMay ∷ DateImprecise → Maybe (Year,Month)
monthMay (DateMonth (y,m)) = Just (y,m)
monthMay _                 = Nothing

----------

{- | Pattern (de)constructor for a day from Y/M. -}
pattern MonthDate ∷ Year → Month → DateImprecise
pattern MonthDate y m ← (monthMay → Just (y,m))
                        where MonthDate y m = dateMonth y m

----------------------------------------

yearMay ∷ DateImprecise → Maybe Year
yearMay (DateYear y) = Just y
yearMay _            = Nothing

{- | Pattern (de)constructor for a day from Y/M. -}
pattern YearDate ∷ Year → DateImprecise
pattern YearDate y ← (yearMay → Just y)
                     where YearDate y = dateYear y

----------------------------------------

endDayOfM ∷ Year → Month → DayOfM
endDayOfM y m = let (_,_,d) = toGregorian $ endDateOfMonth y m
                 in __fromI d
cdayToDate ∷ Day → (Year,Month,DayOfM)
cdayToDate cday = let (y,m,d) = toGregorian cday
                   in (__fromI y, __fromI m, __fromI d)

{- | Convert a `DateImprecise` to a day (the first available day in that
     "range"); as a (y,m,d) triple. -}
toDate ∷ DateImprecise → (Year,Month,DayOfM)
toDate datep = let cday = startDay datep
                in cdayToDate cday

toGregory ∷ Day → (Year,Month,DayOfM)
toGregory d = let (y,m,dom) = toGregorian d
               in (__fromI y, __fromI m, __fromI dom)

----------------------------------------

dateImprecise ∷ QuasiQuoter
dateImprecise =
  mkQQ "DateImprecise" $ def & exp ⊩ ((\ d → ⟦d⟧) ⩺ fromString @DateImprecise)

-- testing ---------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "DateImprecise" [ ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
