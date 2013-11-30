-----------------------------------------------------------------------------
-- Declaration of the MonadRSS class.
--
-- This is a variant of the classic "Control.Monad.RWS" transformer,
-- where the Writer part rides with the State part.
--
-----------------------------------------------------------------------------

module Control.Monad.RSS (
    module Control.Monad.RSS.Strict
  ) where
 
import Control.Monad.RSS.Strict
