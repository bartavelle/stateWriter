-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.RSS
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  bartavelle@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- Declaration of the MonadRSS class.
--
-- This is a variant of the classic "Control.Monad.RWS" transformer,
-- where the Writer part rides with the State part.
--
-----------------------------------------------------------------------------

module Control.Monad.RSS (
    module Control.Monad.RSS.Lazy
  ) where
 
import Control.Monad.RSS.Lazy
