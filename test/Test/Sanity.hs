{-# OPTIONS_GHC -fplugin=TypeLet #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wwarn=deferred-type-errors #-}

module Test.Sanity (tests) where

import Control.Exception
import Data.Functor.Identity

import TypeLet

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Test.Sanity" [
      testGroup "valid" [
          testProperty  "reflexive"   $ castIsId castReflexive
        , testProperty  "singleLet"   $ castIsId castSingleLet
        , testProperty  "singleLetAs" $ castIsId castSingleLetAs
        ]
    , testGroup "invalid" [
          testCase "notEqual" $ expectTypeError castNotEqual
        ]
    ]

castIsId :: (Eq a, Show a, Arbitrary a) => (a -> a) -> a -> Property
castIsId f x = x === f x

expectTypeError :: forall a. a -> Assertion
expectTypeError x = do
    mErr :: Either SomeException a <- try $ evaluate x
    case mErr of
      Left _err -> return () -- TODO: Should we be more precise?
      Right _a' -> assertFailure "Expected type error"

{-------------------------------------------------------------------------------
  Simple casts
-------------------------------------------------------------------------------}

-- | Trivial test: no let-bounds
--
-- TODO: We should also make sure that non-type correct functions are rejected
-- (they are, just don't have a test for them currently)
castReflexive :: Int -> Int
castReflexive = castEqual

-- | Test that we cannot cast between different types
castNotEqual :: Int -> Bool
castNotEqual = castEqual

-- | Introduce single let binding, then cast there and back
castSingleLet :: Int -> Int
castSingleLet x =
    case letT (Proxy @Int) of
      LetT (_p :: Proxy t1) ->
        let y :: t1
            y = castEqual x
        in castEqual y

-- | Single let-as
castSingleLetAs :: Identity Int -> Identity Int
castSingleLetAs x =
    case letAs x of
      LetAs (x' :: Identity t1) ->
        castEqual x'