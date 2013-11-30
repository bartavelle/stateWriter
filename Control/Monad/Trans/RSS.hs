-----------------------------------------------------------------------------
-- A monad transformer that combines 'ReaderT', 'WriterT' and 'StateT'.
-- This version is strict; for a lazy version, see
-- "Control.Monad.Trans.RSS.Lazy", which has the same interface.
--
-- This is a variant of the classic "Control.Monad.Trans.RWS" transformer,
-- where the Writer part rides with the State part.
-----------------------------------------------------------------------------

module Control.Monad.Trans.RSS (
    module Control.Monad.Trans.RSS.Strict
  ) where

import Control.Monad.Trans.RSS.Strict
