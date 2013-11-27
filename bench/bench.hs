{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified Control.Monad.Trans.RSS.Lazy as RSSL
import qualified Control.Monad.Trans.RSS.Strict as RSSS
import qualified Control.Monad.Trans.RWS.Lazy as RWSL
import qualified Control.Monad.Trans.RWS.Strict as RWSS

import Criterion
import Criterion.Main

import qualified Data.Sequence as Seq
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V

import Control.Monad.RWS

testActions :: (Monoid w, Monad m, MonadRWS () w Int m) => (Int -> m ()) -> m ()
testActions tellaction = do
    v <- get
    unless (v == 0) $ do
        put $! v - 1
        when (v `mod` 11 == 0) $ tellaction v
        testActions tellaction

benchlen :: Int
benchlen = 10000

actions :: (Monoid w) => (Int -> w) -> [(String, Int -> ((), Int, w))]
actions cnv = [ ("RSS.Lazy"  , RSSL.runRSS (testActions (tell . cnv)) ())
              , ("RSS.Strict", RSSS.runRSS (testActions (tell . cnv)) ())
              , ("RWS.Lazy"  , RWSL.runRWS (testActions (tell . cnv)) ())
              , ("RWS.Strict", RWSS.runRWS (testActions (tell . cnv)) ())
              ]

main :: IO ()
main = defaultMain $ mkBench "Seq" Seq.singleton
                  ++ mkBench "List" (:[])
                  ++ mkBench "Vector Primitive" VP.singleton
                  ++ mkBench "Vector Storable" VS.singleton
                  ++ mkBench "Vector Unboxed" VU.singleton
                  ++ mkBench "Vector" V.singleton
    where
        mkBench n = map toBench . actions
            where
                toBench (n', a) = bench (n' ++ " [" ++ n ++ "]") $ nf a benchlen

