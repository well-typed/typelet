{-# OPTIONS_GHC -fplugin=TypeLet #-}
-- {-# OPTIONS_GHC -ddump-ds-preopt -ddump-ds -ddump-simpl -ddump-to-file #-}
{-# OPTIONS_GHC -ddump-tc-trace -ddump-to-file #-}

module Test.Sanity (tests) where

import Data.Functor.Identity
import Data.Kind

import TypeLet

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Test.Infra

tests :: TestTree
tests = testGroup "Test.Sanity" [] {-
      testGroup "simple" [
          testProperty  "reflexive"   $ castIsId castReflexive
        , testProperty  "singleLet"   $ castIsId castSingleLet
        , testProperty  "singleLetAs" $ castIsId castSingleLetAs
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

testHList :: HList '[A, B, C] -> Assertion
testHList =
    assertEqual "" hlistBaseline

testAp ::
     (forall f r. Applicative f => (A -> B -> C -> r) -> f r)
  -> Assertion
testAp apTest =
    assertEqual "" (runIdentity $ apBaseline f) (runIdentity $ apTest f)
  where
    f :: A -> B -> C -> HList '[A, B, C]
    f x y z = HCons x $ HCons y $ HCons z $ HNil
-}

{-------------------------------------------------------------------------------
  Simple casts
-------------------------------------------------------------------------------}

{-
-- | Trivial test: no let-bounds
--
-- TODO: We should also make sure that non-type correct functions are rejected
-- (they are, just don't have a test for them currently)
castReflexive :: Int -> Int
castReflexive = castEqual
-}

-- | Introduce single let binding, then cast there and back
castSingleLet :: Int -> Int
castSingleLet x =
    -- TODO: the kind annotation is necessary because without it, the
    -- unification variable t1 does not unify with the skolem variable for
    -- the existential before the plugin runs, and as a consequence when the
    -- plugin applies the substitution, nothing happens (the substitution would
    -- replace the skolem variable with @Int@, but since @t1@ hasn't unified
    -- with that skolem variable yet, nothing happens).
    case letT @Type (Proxy) of -- @Int) of
      LetT (_ :: t1 := Int) ->
        let y :: t1
            y = castEqual x
        in undefined -- castEqual y

{-
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

{-# NOINLINE hlistBaseline #-}
hlistBaseline :: HList '[A, B, C]
hlistBaseline =
      HCons A
    $ HCons B
    $ HCons C
    $ HNil

hlistLetAs :: HList '[A, B, C]
hlistLetAs =
    case letAs (HCons C HNil) of { LetAs (xs02 :: HList t02) ->
    case letAs (HCons B xs02) of { LetAs (xs01 :: HList t01) ->
    case letAs (HCons A xs01) of { LetAs (xs00 :: HList t00) ->
      castEqual xs00
    }}}

hlistLetAsCPS :: HList '[A, B, C]
hlistLetAsCPS =
    letT' (Proxy @'[A, B, C]) $ \(_ :: Proxy r) -> castEqual $
      letAs' @(HList r) (HCons C HNil) $ \(xs02 :: HList t02) ->
      letAs' @(HList r) (HCons B xs02) $ \(xs01 :: HList t01) ->
      letAs' @(HList r) (HCons A xs01) $ \(xs00 :: HList t00) ->
        castEqual xs00

{-------------------------------------------------------------------------------
  Similarly, small versions of the @<*>@ tests.
-------------------------------------------------------------------------------}

apBaseline :: Applicative f => (A -> B -> C -> r) -> f r
apBaseline f =
        pure f
    <*> pure A
    <*> pure B
    <*> pure C

apLet :: forall f r. Applicative f => (A -> B -> C -> r) -> f r
apLet f =
    case letT (Proxy @(C -> r))   of { LetT (_ :: Proxy l02) ->
    case letT (Proxy @(B -> l02)) of { LetT (_ :: Proxy l01) ->
    case letT (Proxy @(A -> l01)) of { LetT (_ :: Proxy l00) ->

      let f00 :: f l00
          f01 :: f l01
          f02 :: f l02

          res :: f r

          f00 = pure (castEqual f)
          f01 = castEqual f00 <*> pure A
          f02 = castEqual f01 <*> pure B
          res = castEqual f02 <*> pure C

      in res

    }}}
-}