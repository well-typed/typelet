{-# OPTIONS_GHC -fplugin=TypeLet #-}
-- {-# OPTIONS_GHC -fdefer-type-errors -Wwarn=deferred-type-errors #-}

module Test.Sanity (tests) where

import Control.Exception
import Data.Functor.Identity

import TypeLet

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Test.Size.Setup

tests :: TestTree
tests = testGroup "Test.Sanity" [
      testGroup "simple" [
          testProperty  "reflexive"   $ castIsId castReflexive
        , testProperty  "singleLet"   $ castIsId castSingleLet
        , testProperty  "singleLetAs" $ castIsId castSingleLetAs
        ]
    , testGroup "invalid" [
          testCase "notEqual" $ expectTypeError castNotEqual
        ]
    , testGroup "HList" [
          testCase "LetAs"    $ testHList hlistLetAs
        , testCase "LetAsCPS" $ testHList hlistLetAsCPS
        ]
    , testGroup "Ap" [
          testCase "Let" $ testAp apLet
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

testHList :: HList '[T "i00", T "i01", T "i02"] -> Assertion
testHList =
    assertEqual "" hlistBaseline

testAp ::
     (forall f r. Applicative f => (T "i00" -> T "i01" -> T "i02" -> r) -> f r)
  -> Assertion
testAp apTest =
    assertEqual "" (runIdentity $ apBaseline f) (runIdentity $ apTest f)
  where
    f :: T "i00" -> T "i01" -> T "i02" -> HList '[T "i00", T "i01", T "i02"]
    f x y z = HCons x $ HCons y $ HCons z $ HNil

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
castNotEqual = undefined -- castEqual

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

{-------------------------------------------------------------------------------
  Small versions of the 'HList' tests that we use for size measurements

  Since these work with only 3 indices, they are a bit more manageable for
  easy experimentation.
-------------------------------------------------------------------------------}

hlistBaseline :: HList '[T "i00", T "i01", T "i02"]
hlistBaseline =
      -- 00 .. 09
      HCons (MkT @"i00")
    $ HCons (MkT @"i01")
    $ HCons (MkT @"i02")
    $ HNil

hlistLetAs :: HList '[T "i00", T "i01", T "i02"]
hlistLetAs =
    case letAs (HCons (MkT @"i02") HNil) of { LetAs (xs02 :: HList t02) ->
    case letAs (HCons (MkT @"i01") xs02) of { LetAs (xs01 :: HList t01) ->
    case letAs (HCons (MkT @"i00") xs01) of { LetAs (xs00 :: HList t00) ->
      castEqual xs00
    }}}

hlistLetAsCPS :: HList '[T "i00", T "i01", T "i02"]
hlistLetAsCPS =
    letT' (Proxy @'[T "i00", T "i01", T "i02"]) $ \(_ :: Proxy r) -> castEqual $
      letAs' @(HList r) (HCons (MkT @"i02") HNil) $ \(xs02 :: HList t02) ->
      letAs' @(HList r) (HCons (MkT @"i01") xs02) $ \(xs01 :: HList t01) ->
      letAs' @(HList r) (HCons (MkT @"i00") xs01) $ \(xs00 :: HList t00) ->
        castEqual xs00

{-------------------------------------------------------------------------------
  Similarly, small versions of the @<*>@ tests.
-------------------------------------------------------------------------------}

apBaseline :: Applicative f => (T "i00" -> T "i01" -> T "i02" -> r) -> f r
apBaseline f =
        pure f
    <*> pure (MkT @"i00")
    <*> pure (MkT @"i01")
    <*> pure (MkT @"i02")

apLet :: forall f r. Applicative f => (T "i00" -> T "i01" -> T "i02" -> r) -> f r
apLet f =
    case letT (Proxy @(T "i02" -> r))   of { LetT (_ :: Proxy l02) ->
    case letT (Proxy @(T "i01" -> l02)) of { LetT (_ :: Proxy l01) ->
    case letT (Proxy @(T "i00" -> l01)) of { LetT (_ :: Proxy l00) ->

      let f00 :: f l00
          f01 :: f l01
          f02 :: f l02

          res :: f r

          f00 = pure (castEqual f)
          f01 = castEqual f00 <*> pure (MkT @"i00")
          f02 = castEqual f01 <*> pure (MkT @"i01")
          res = castEqual f02 <*> pure (MkT @"i02")

      in res

    }}}
