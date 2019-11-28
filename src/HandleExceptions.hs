{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HandleExceptions where

import Control.Monad
import Control.Monad.Catch
import Data.Typeable


newtype MyException = MyException String deriving (Show, Typeable)
instance Exception MyException


isA, isB :: IO Bool
isA = return False
isB = return False

run :: IO ()
run = do
    a <- isA
    b <- isB
    when a $
        throwM $ MyException "isA"
    when b $
        throwM $ MyException "isB"
    print "yes"

someFunc :: IO ()
someFunc =
    run `catch` \(SomeException e) -> do
        print "caught"
        print e
