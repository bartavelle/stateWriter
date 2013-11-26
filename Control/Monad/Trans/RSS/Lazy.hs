{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Control.Monad.Trans.RSS.Lazy where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Functor.Identity

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS

type RSS r w s = RSST r w s Identity

runRSS :: RSS r w s a -> r -> s -> (a,s,[w])
runRSS m r s =
    let (a,(s',w)) = runIdentity (runRSST m r (s, []))
    in  (a,s',w)

newtype RSST r w s m a = RSST { runRSST :: r -> (s,[w]) -> m (a, (s, [w])) }

-- | Evaluate a computation with the given initial state and environment,
-- returning the final value and output, discarding the final state.
evalRSST :: (Monad m)
            => RSST r w s m a     -- ^computation to execute
            -> r                  -- ^initial environment
            -> s                  -- ^initial value
            -> m (a,[w])          -- ^computation yielding final value and output
evalRSST m r s = do
    ~(a, (_, w)) <- runRSST m r (s,[])
    return (a, reverse w)

-- | Evaluate a computation with the given initial state and environment,
-- returning the final state and output, discarding the final value.
execRSST :: (Monad m)
            => RSST r w s m a      -- ^computation to execute
            -> r                   -- ^initial environment
            -> s                   -- ^initial value
            -> m (s, [w])          -- ^computation yielding final state and output
execRSST m r s = do
        ~(_, (s', w)) <- runRSST m r (s,[])
        return (s', reverse w)

instance (Functor m) => Functor (RSST r w s m) where
    fmap f m = RSST $ \r s ->
        fmap (\ ~(a, (s', w)) -> (f a, (s', w))) $ runRSST m r s

instance (Monad m) => Monad (RSST r w s m) where
    return a = RSST $ \_ s -> return (a, s)
    m >>= k  = RSST $ \r s -> do
        ~(a, (s', w))  <- runRSST m r s
        ~(b, (s'',w')) <- runRSST (k a) r (s',w)
        return (b, (s'', w'))
    fail msg = RSST $ \_ _ -> fail msg

instance (MonadPlus m) => MonadPlus (RSST r w s m) where
    mzero       = RSST $ \_ _ -> mzero
    m `mplus` n = RSST $ \r s -> runRSST m r s `mplus` runRSST n r s

instance (Functor m, Monad m) => Applicative (RSST r w s m) where
    pure = return
    (<*>) = ap

instance (Functor m, MonadPlus m) => Alternative (RSST r w s m) where
    empty = mzero
    (<|>) = mplus

instance (MonadFix m) => MonadFix (RSST r w s m) where
    mfix f = RSST $ \r s -> mfix $ \ ~(a, _) -> runRSST (f a) r s

instance MonadTrans (RSST r w s) where
    lift m = RSST $ \_ s -> do
        a <- m
        return (a, s)

instance (MonadIO m) => MonadIO (RSST r w s m) where
    liftIO = lift . liftIO

instance Monad m => MonadState s (RSST r w s m) where
    get = RSST $ \_ (s,w) -> return (s,(s,w))
    put ns = RSST $ \_ (_,w) -> return ((),(ns,w))
    state f = RSST $! \_ (s,w) -> case f s of
                                      (a,s') -> return (a, (s', w))



instance Monad m => MonadReader r (RSST r w s m) where
    ask = RSST $! \r s -> return (r, s)
    local f rw = RSST $! \r s -> runRSST rw (f r) s
    reader f = RSST $! \r s -> return (f r, s)

instance Monad m => MonadWriter [w] (RSST r w s m) where
    writer (a,w) = tell w >> return a
    tell w = RSST $! \_ (s, ow) -> return ((), (s, ow ++ reverse w))
    listen rw = RSST $! \r s -> do
        (a, (ns, nw)) <- runRSST rw r s
        return ((a, nw), (ns, nw))
    pass rw = RSST $! \r s -> do
        ( (a, fw), (s', w) ) <- runRSST rw r s
        return (a, (s', fw w))

instance Monad m => MonadRWS r [w] s (RSST r w s m)

