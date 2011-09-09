{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Toys where

import Control.Monad (liftM)
import Control.Monad.Error (MonadError)

import Erl

data ToyData = ToyData { name :: String, age :: Int } deriving (Show)

toyData :: String -> Int -> ToyData
toyData = ToyData


newtype Toys a = Toys { runToys :: ErlMonad ToyData a }
               deriving (Monad, MonadError ErlError, MonadErl ToyData)

doToys :: Toys a -> ErlState ToyData -> (Either ErlError a, ErlState ToyData)
doToys m s = doErl (runToys m) s

doToys' :: Toys a -> ErlState ToyData -> (a, ErlState ToyData)
doToys' m s = let (errOrResult, s') = doToys m s
              in either (error . show) (,s') errOrResult

evalToys :: Toys a -> ErlState ToyData -> Either ErlError a
evalToys m s = evalErl (runToys m) s

evalToys' :: Toys a -> ErlState ToyData -> a
evalToys' m s = either (error . show) id $  evalToys m s

evalToys'' :: Toys a -> a
evalToys'' m = evalToys' m initial

molly, gordon, milo, lucy, lola, boris :: EntityId
initial :: ErlState ToyData

createToy :: String -> Int -> Toys EntityId
createToy name age = createEntity $ toyData name age

getName :: EntityId -> Toys String
getName eid = (name . attributes) `liftM` entity eid

getAge :: EntityId -> Toys Int
getAge eid = (age . attributes) `liftM` entity eid

([molly, gordon, milo, lucy, lola, boris], initial) =
  flip doToys' emptyState $
    sequence [createToy "Molly" 20,
              createToy "Gordon" 45,
              createToy "Milo" 7,
              createToy "Lucy" 14,
              createToy "Lola" 5,
              createToy "Boris" 33]
