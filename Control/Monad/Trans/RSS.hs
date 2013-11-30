-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.RSS
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  bartavelle@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A monad transformer that combines 'ReaderT', 'WriterT' and 'StateT'.
-- This version is lazy; for a strict version, see
-- "Control.Monad.Trans.RSS.Strict", which has the same interface.
--
-- This is a variant of the classic "Control.Monad.Trans.RWS" transformer,
-- where the Writer part rides with the State part.
-----------------------------------------------------------------------------

module Control.Monad.Trans.RSS (
    module Control.Monad.Trans.RSS.Lazy
  ) where

import Control.Monad.Trans.RSS.Lazy
