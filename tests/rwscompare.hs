{-# LANGUAGE GADTs, FlexibleContexts #-}
module Main where

import Control.Monad.Trans.RSS.Strict
import Control.Monad.RWS
import Test.Hspec
import Test.QuickCheck
import Control.Applicative

data ActionF next = Tell        [Int]  (ActionF next)
                  | SetState    Int    (ActionF next)
                  | AskAndStore IModification (ActionF next)
                  | Modify      SModification (ActionF next)
                  | GetAndStore IModification (ActionF next)
                  | Return      next

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
    arbitrary = oneof [ Tell        <$> arbitrary <*> arbitrary
                      , SetState    <$> arbitrary <*> arbitrary
                      , AskAndStore <$> arbitrary <*> arbitrary
                      , GetAndStore <$> arbitrary <*> arbitrary
                      , Modify      <$> arbitrary <*> arbitrary
                      , Return      <$> arbitrary
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
    show (Return n) = "Return " ++ show n

evaluateActions :: (MonadRWS Int [Int] Int m) => ActionF x -> m x
evaluateActions (Tell x next)        = tell x >>  evaluateActions next
evaluateActions (SetState s next)    = put s  >>  evaluateActions next
evaluateActions (AskAndStore f next) = ask >>= tell . evaluateIM f >> evaluateActions next
evaluateActions (GetAndStore f next) = get >>= tell . evaluateIM f >> evaluateActions next
evaluateActions (Modify f next) = modify (evaluateSM f) >> evaluateActions next
evaluateActions (Return next) = return next

main :: IO ()
main = hspec $ do
    describe "Writer part" $ do
        it "logs stuff in the right order, with tell" $
            property $ \listOfLists -> runRSS (mapM_ tell (listOfLists :: [[Int]])) () () == runRWS (mapM_ tell listOfLists) () ()
        it "logs stuff in the right order, with tellElement" $
            property $ \list -> runRSS (mapM_ tellElement (list :: [Int])) () () == ((), (), list)
        it "interprets actions the same" $
            property $ \actions -> runRSS (evaluateActions (actions :: ActionF Int)) 42 12 == runRWS (evaluateActions actions) 42 12

