{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified Control.Monad.Trans.RSS.Lazy as RSSL
import qualified Control.Monad.Trans.RSS.Strict as RSSS
import qualified Control.Monad.Trans.RWS.Lazy as RWSL
import qualified Control.Monad.Trans.RWS.Strict as RWSS

import Criterion
import Criterion.Main
import Data.Sequence

import Control.Monad.RWS

testActions :: (Monad m, MonadRWS () (Seq Int) Int m) => m ()
testActions = do
    v <- get
    unless (v == 0) $ do
        put $! v - 1
        when (v `mod` 23 == 0) $ tell (singleton v)
        testActions

benchlen :: Int
benchlen = 10000

main :: IO ()
main = defaultMain [ bench "RSS.Lazy"   $ nf (RSSL.runRSS testActions ()) benchlen
                   , bench "RSS.Strict" $ nf (RSSS.runRSS testActions ()) benchlen
                   , bench "RWS.Lazy"   $ nf (RWSL.runRWS testActions ()) benchlen
                   , bench "RWS.Strict" $ nf (RWSS.runRWS testActions ()) benchlen
                   ]
