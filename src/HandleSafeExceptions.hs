module HandleSafeExceptions where

import Control.Exception.Safe
import Control.Monad

{-
    こちらのほうがexceptionsより新しいらしい
-}

isA, isB :: IO Bool
isA = return False
isB = return True

run :: IO ()
run = do
    a <- isA
    b <- isB
    when a $
        throwString "isA"
    when b $
        throwString "isB"
    print "yes"

someFunc :: IO ()
someFunc =
    run `catch` \(SomeException e) -> do
        print "caught"
        print e
