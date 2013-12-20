{-# LANGUAGE GADTs, FlexibleContexts, FlexibleInstances #-}
module Main where

import Control.Monad.Trans.RSS.Strict
import Control.Monad.RWS
import Test.Hspec
import Test.QuickCheck
import Control.Applicative
import Control.Monad.Free

data ActionF next = Tell        [Int]         next
                  | SetState    Int           next
                  | AskAndStore IModification next
                  | Modify      SModification next
                  | GetAndStore IModification next

type Action = Free ActionF

data SModification = SId
                   | Double
                   deriving (Enum, Show, Bounded)

data IModification = IReturn
                   | ReplicateThrice
                   deriving (Enum, Show, Bounded)

instance Arbitrary SModification where
    arbitrary = arbitraryBoundedEnum
instance Arbitrary IModification where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary next => Arbitrary (ActionF next) where
    arbitrary = frequency [ (5,  Tell        <$> arbitrary <*> arbitrary)
                          , (5,  SetState    <$> arbitrary <*> arbitrary)
                          , (20, AskAndStore <$> arbitrary <*> arbitrary)
                          , (20, GetAndStore <$> arbitrary <*> arbitrary)
                          , (20, Modify      <$> arbitrary <*> arbitrary)
                          ]

instance Arbitrary (Action Int) where
    arbitrary = frequency [ (1, Pure <$> arbitrary)
                          , (9, Free <$> arbitrary) 
                          ]

evaluateIM :: IModification -> (Int -> [Int])
evaluateIM IReturn x = [x]
evaluateIM ReplicateThrice x = [x,x,x]

evaluateSM :: SModification -> (Int -> Int)
evaluateSM SId = id
evaluateSM Double = (*) 2

instance Show next => Show (ActionF next) where
    show (Tell x n) = "Tell " ++ show x ++ " / " ++ show n
    show (SetState s n) = "Set " ++ show s ++ " / " ++ show n
    show (AskAndStore i n) = "AskAndStore " ++ show i ++ " / " ++ show n
    show (GetAndStore i n) = "GetAndStore " ++ show i ++ " / " ++ show n
    show (Modify s n) = "Modify " ++ show s ++ " / " ++ show n

evaluateActions :: (MonadRWS Int [Int] Int m) => Action x -> m x
evaluateActions (Free (Tell x next))        = tell x >>  evaluateActions next
evaluateActions (Free (SetState s next))    = put s  >>  evaluateActions next
evaluateActions (Free (AskAndStore f next)) = ask >>= tell . evaluateIM f >> evaluateActions next
evaluateActions (Free (GetAndStore f next)) = get >>= tell . evaluateIM f >> evaluateActions next
evaluateActions (Free (Modify f next)) = modify (evaluateSM f) >> evaluateActions next
evaluateActions (Pure x) = return x

main :: IO ()
main = hspec $ do
    describe "Writer part" $ do
        it "logs stuff in the right order, with tell" $
            property $ \listOfLists -> runRSS (mapM_ tell (listOfLists :: [[Int]])) () () == runRWS (mapM_ tell listOfLists) () ()
        it "interprets actions the same" $
            property $ \actions -> runRSS (evaluateActions (actions :: Action Int)) 42 12 == runRWS (evaluateActions actions) 42 12

