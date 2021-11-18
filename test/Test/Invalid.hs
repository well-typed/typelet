{-# OPTIONS_GHC -fplugin=TypeLet #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wwarn=deferred-type-errors #-}

-- | Test that invalid casts are detected as such
module Test.Invalid (tests) where

import Control.Exception

import TypeLet

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Test.Invalid" [
      testCase "notEqual" $ expectTypeError castNotEqual
    ]

expectTypeError :: forall a. a -> Assertion
expectTypeError x = do
    mErr :: Either SomeException a <- try $ evaluate x
    case mErr of
      Left _err -> return () -- TODO: Should we be more precise?
      Right _a' -> assertFailure "Expected type error"

castNotEqual :: Int -> Bool
castNotEqual = castEqual


