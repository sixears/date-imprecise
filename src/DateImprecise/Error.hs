{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnicodeSyntax         #-}

{- | Date, with varying precisions - day, month, year. -}

module DateImprecise.Error
  ( AsDateError_( _DateError_ ), DateError_, DateErrorImprecise
  , badDateError, dateRangeError, dateRangeError_, dateRangesOverlap
  , dateRangesOverlap_, emap )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Eq            ( Eq )
import Data.Function      ( ($), id )
import Data.Typeable      ( Typeable )
import Text.Show          ( Show )

-- date-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- lens --------------------------------

import Control.Lens.Prism   ( Prism' )
import Control.Lens.Review  ( (#) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import DateImprecise.DateImpreciseType  ( DateImprecise )

--------------------------------------------------------------------------------

data Show ρ ⇒ DateError_ ρ = BadDateError ρ
                           | DateRangeError (ρ,ρ)
                           | DateRangesOverlap (ρ,ρ)
  deriving (Eq, Show)

instance (Show ρ, Typeable ρ) ⇒ Exception (DateError_ ρ)

instance (Printable ρ, Show ρ) ⇒ Printable (DateError_ ρ) where
  print (BadDateError d)         = P.text $ [fmt|Bad Date: %T|] d
  print (DateRangeError (d0,d1)) = P.text $ [fmt|Bad Date Range: %T → %T|] d0 d1
  print (DateRangesOverlap (r0,r1)) =
                              P.text $ [fmt|Date Ranges Overlap: %T → %T|] r0 r1

class AsDateError_ ρ ε where
  _DateError_ ∷ Prism' ε (DateError_ ρ)

instance AsDateError_ ρ (DateError_ ρ) where
  _DateError_ = id

badDateError ∷ (Show ρ, AsDateError_ ρ ε, MonadError ε η) ⇒ ρ → η α
badDateError x = throwError $ _DateError_ # BadDateError x

dateRangeError ∷ (Show ρ, AsDateError_ ρ ε, MonadError ε η) ⇒ ρ → ρ → η α
dateRangeError d0 d1 = throwError $ _DateError_ # DateRangeError (d0,d1)

dateRangeError_ ∷ (Show ρ, AsDateError_ ρ ε) ⇒ ρ → ρ → ε
dateRangeError_ d0 d1 = _DateError_ # DateRangeError (d0,d1)

dateRangesOverlap ∷ (Show ρ, AsDateError_ ρ ε, MonadError ε η) ⇒ ρ → ρ → η α
dateRangesOverlap r0 r1 = throwError $ _DateError_ # DateRangesOverlap (r0,r1)

dateRangesOverlap_ ∷ (Show ρ, AsDateError_ ρ ε) ⇒ ρ → ρ → ε
dateRangesOverlap_ r0 r1 = _DateError_ # DateRangesOverlap (r0,r1)

emap ∷ (Show α, Show β) ⇒ (α → β) → DateError_ α → DateError_ β
emap f (BadDateError d) = BadDateError (f d)
emap f (DateRangeError (d0,d1)) = DateRangeError (f d0, f d1)
emap f (DateRangesOverlap (r0,r1)) = DateRangesOverlap (f r0, f r1)

------------------------------------------------------------

type DateErrorImprecise = DateError_ DateImprecise

-- that's all, folks! ----------------------------------------------------------
