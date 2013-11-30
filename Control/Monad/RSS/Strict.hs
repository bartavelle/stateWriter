-----------------------------------------------------------------------------
-- |
-- Strict RSS monad.
--
-- This is a variant of the classic "Control.Monad..RWS.Strict" transformer,
-- where the Writer part rides with the State part.
--
-----------------------------------------------------------------------------

module Control.Monad.RSS.Strict (
    -- * The RSS monad
    RSS,
    rss,
    runRSS,
    evalRSS,
    execRSS,
    withRSS,
    -- * The RSST monad transformer
    RSST,
    evalRSST,
    execRSST,
    withRSST,
    -- * Strict Reader-writer-state monads
    module Control.Monad.RWS.Class,
    module Control.Monad,
    module Control.Monad.Fix,
    module Control.Monad.Trans,
    module Data.Monoid,
  ) where

import Control.Monad.RWS.Class

import Control.Monad.Trans
import Control.Monad.Trans.RSS.Strict (
    RSS, rss, runRSS, evalRSS, execRSS, withRSS,
    RSST, evalRSST, execRSST, withRSST)

import Control.Monad
import Control.Monad.Fix
import Data.Monoid

