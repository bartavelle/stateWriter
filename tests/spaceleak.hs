
module Main where

import qualified Control.Monad.Trans.RSS.Lazy as RSSL
import qualified Control.Monad.Trans.RSS.Strict as RSSS
import Control.Monad.Writer

n :: Int
n = 10000000

main :: IO ()
main = do
    print $ RSSS.runRSS (replicateM_ n $ tell $ Sum (1 :: Int)) () ()
    putStrLn "Strict version ok, the next test should explode the stack."
    print $ RSSL.runRSS (replicateM_ n $ tell $ Sum (1 :: Int)) () ()
    putStrLn "Lazy version have exploded"
