{-# OPTIONS_GHC -fplugin=TypeLet #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wwarn=deferred-type-errors #-}

-- | Test that invalid casts are detected as such
module Test.Invalid (tests) where

import Control.Exception
import Data.List (isInfixOf)

import TypeLet

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Test.Invalid" [
      testCase "notEqual" $ expectException intBoolMismatch castNotEqual
    ]
  where
    intBoolMismatch :: String -> Bool
    intBoolMismatch e = "Int" `isInfixOf` e && "Bool" `isInfixOf` e

expectException :: forall a. (String -> Bool) -> a -> Assertion
expectException p x = do
    mErr :: Either SomeException a <- try $ evaluate x
    case mErr of
      Left err ->
        assertBool "Exception does not match predicate" (p $ show err)
      Right _a' ->
        assertFailure $ "Expected exception, but got value"

castNotEqual :: Int -> Bool
castNotEqual = castEqual


