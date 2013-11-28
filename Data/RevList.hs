{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Data.RevList where

import Prelude hiding (null,init,last)
import qualified Prelude as Prelude
import Data.Monoid
import Control.DeepSeq
import Control.Lens hiding (cons,snoc)
import qualified Control.Lens as L
import Control.Lens.Cons ()
import Control.Applicative

newtype RevList a = RevList [a]

unRevlist :: RevList a -> [a]
unRevlist (RevList x) = reverse x

instance Monoid (RevList a) where
    mempty = RevList mempty
    {-# INLINE mempty #-}
    RevList [] `mappend` RevList b = RevList b
    RevList a `mappend` RevList [] = RevList a
    RevList a `mappend` RevList b = RevList $! b <> a
    {-# INLINE mappend #-}

instance NFData a => NFData (RevList a) where
    rnf (RevList a) = rnf a

singleton :: a -> RevList a
singleton = RevList . (:[])
{-# INLINE singleton #-}

null :: RevList a -> Bool
null (RevList []) = True
null _ = False
{-# INLINE null #-}

init :: RevList a -> RevList a
init (RevList x) = RevList (Prelude.init x)
{-# INLINE init #-}

last :: RevList a -> a
last (RevList x) = Prelude.last x
{-# INLINE last #-}

cons :: a -> RevList a -> RevList a
cons x (RevList xs) = RevList (L.snoc xs x)
{-# INLINE cons #-}

snoc :: RevList a -> a -> RevList a
snoc (RevList xs) x = RevList (x:xs)
{-# INLINE snoc #-}

instance (Choice p, Applicative f) => Cons p f (RevList a) (RevList b) a b where
    _Cons = prism (uncurry cons) $ \aas -> if null aas
       then Left mempty
       else Right (last aas, init aas)
    {-# INLINE _Cons #-}

instance (Choice p, Applicative f) => Snoc p f (RevList a) (RevList b) a b where
    _Snoc = prism (uncurry snoc) $ \aas -> case aas of
           RevList (a:as) -> Right (RevList as, a)
           RevList []     -> Left  mempty
    {-# INLINE _Snoc #-}
