
module Main where

import qualified Control.Monad.Trans.RSS.Lazy as RSSL
import qualified Control.Monad.Trans.RSS.Strict as RSSS
import System.Environment
import Control.Monad.Writer

n :: Int
n = 10000000

main :: IO ()
main = do
    print $ RSSS.runRSS (replicateM_ n $ tell $ Sum (1 :: Int)) () ()
    tryExplode <- fmap (not . null) getArgs
    if tryExplode
        then do
            putStrLn "Strict version ok, the next test should explode the stack."
            print $ RSSL.runRSS (replicateM_ n $ tell $ Sum (1 :: Int)) () ()
            putStrLn "Lazy version should have exploded !"
        else
            putStrLn "Do not try exploding the stack. Run the test program with any command line argument to test it"
